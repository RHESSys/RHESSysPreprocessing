#' World Gen
#'
#' Generates world files for use in RHESSys based on input template and maps.
#' @param worldfile Name and path of worldfile to be created.
#' @inheritParams RHESSysPreprocess
#' @seealso \code{\link{raster}}
#' @author Will Burke
#' @importFrom stats aggregate
#' @export

world_gen = function(template,
                     worldfile,
                     type = 'Raster',
                     typepars,
                     overwrite = FALSE,
                     header = FALSE,
                     unique_strata_ID = TRUE,
                     asprules = NULL,
                     seq_patch_IDs = FALSE,
                     output_patch_map = FALSE,
                     wrapper = FALSE) {

  # -------------------- Input & Error Checking --------------------
  options(scipen = 999) # no scientific notation - prevents automatic conversion later

  if (!file.exists(template)) {
    print(paste("Template does not exist or is not located at specified path:",template),quote = FALSE) #check if template exists
  }

  if (!is.logical(overwrite)) {stop("overwrite must be logical")} # check overwrite inputs
  if (file.exists(worldfile) & overwrite == FALSE) {stop(noquote(paste("Worldfile",worldfile,"already exists.")))}

  if (!is.null(asprules)) {asp_check = TRUE} else {asp_check = FALSE} # check for aspatial patches
  if (asp_check) { if (!file.exists(asprules) ) {asp_check = FALSE}}

  # ----- File Name Check+Conversion -----
  worldname = basename(worldfile)# Coerce .world extension
  if (startsWith(worldname,"World.") | startsWith(worldname,"world.")) {
    worldname = paste(substr(worldname,7,nchar(worldname)),".world",sep = "")
  } else if (!endsWith(worldname,".world")) {
    worldname = paste(worldname,".world",sep = "")
  }
  worldfile = file.path(dirname(worldfile),worldname)

  # -------------------- Read in Template --------------------
  template_list = template_read(template)

  template_clean = template_list[[1]] # template in list form
  var_names = template_list[[2]] # names of template vars
  level_index = template_list[[3]] # index of level separators in template_clean/var_names
  var_index = template_list[[4]] # index of vars
  map_info = template_list[[5]] # tables of maps and their inputs/names in the template
  head = template_list[[6]] # header
  maps_in = unique(map_info[,2])

  # if using aspatial patches, check for aspatial rules state variable in template
  if (asp_check) {
    if (sum(var_names == "asp_rule") < 1) {
      stop(noquote("Missing asp_rule state variable in template"))
    }
  }

  # -------------------- Read in Maps --------------------
  read_maps = GIS_read(
    maps_in = maps_in,
    type = type,
    typepars = typepars,
    map_info = map_info,
    seq_patch_IDs = seq_patch_IDs,
    output_patch_map = output_patch_map
  )

  # process map data
  if (length(read_maps@data[,1]) == 1) {
    map_df = as.data.frame(read_maps@data) # works for 1 patch world
  } else {
    map_df = as.data.frame(read_maps) #make data frame for ease of use
  }

  cell_len = read_maps@grid@cellsize[1] # cell length for output
  cellarea = read_maps@grid@cellsize[1] * read_maps@grid@cellsize[2] # get cell area - need for area operator
  cellarea = rep(cellarea, length(map_df[,1]))
  rm(read_maps) # read_maps may be very large if maps are large

  # structure to iterate through spatial levels ---- matrix with unique ID's for each unit at each level
  w_map = map_info[map_info[,1] == "world",2]
  b_map = map_info[map_info[,1] == "basin",2]
  h_map = map_info[map_info[, 1] == "hillslope", 2]
  z_map = map_info[map_info[, 1] == "zone", 2]
  p_map = map_info[map_info[, 1] == "patch", 2]
  s_map = map_info[map_info[, 1] == "strata", 2]
  #levels = unname(data.matrix(map_df[c(w_map,b_map,h_map,z_map,p_map,s_map)], length(map_df[p_map])))
  # if we run into memory issues, re factor this code

  levels = data.matrix(map_df[c(w_map,b_map,h_map,z_map,p_map,s_map)], length(map_df[p_map]))
  levels = as.data.frame(levels)
  colnames(levels) = c("w", "b", "h", "z", "p", "s")

  # mode for aggregating by mode
  mode_fun = function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  # -------------------- Aspatial Patch Processing --------------------
  rulevars = NULL
  if (asp_check) {
    asp_map = template_clean[[which(var_names == "asp_rule")]][3] # get rule map/value
    if (suppressWarnings(any(is.na(as.numeric(asp_map))))) { # if it's a map
      asp_mapdata = map_df[asp_map]
    } else if (suppressWarnings(all(!is.na(as.numeric(asp_map))))) { # if is a single number
      asp_mapdata = as.numeric(asp_map)
    }
    rulevars = aspatial_patches(asprules = asprules, asp_mapdata = asp_mapdata)

    if (is.data.frame(asp_mapdata)) { # add ruleID to levels df
      asp_level = aggregate(asp_mapdata[[1]] , by = as.list(levels), FUN = mode_fun)
      levels = merge(levels, asp_level[,c("h", "z", "p", "x")], by = c("h", "z", "p"), sort = F)
      levels = levels[c("w", "b", "h", "z", "p", "s", "x")]
      names(levels)[7] = "a"
      #levels = cbind(levels, a = unname(as.matrix(asp_mapdata)))
    } else if (is.numeric(asp_mapdata)) {
      levels = cbind(levels, a = rep(asp_mapdata,length(levels[,1])) )
    }
    level_index = c(level_index, length(template_clean)+2) # because rules got added to the levels matrix, this is to prevent them from being aggregated across
  }

  # -------------------- Additional Input Checking --------------------
  # Error checking for basestationID/number - it's a loop :/ but short so not a big deal
  # If n_basestations is 0 -> do NOT include base_station_ID
  # If n_basestations is > 0 -> must include base_station_ID, even if doing redefine w/ -9999
  n_basestations_index = which(endsWith(var_names, "n_basestations"))
  if (length(n_basestations_index) != 5) {stop("Could not find state variable 'n_basestations' at each level, variable must be present - check template")}

  # check base_station to basestation since thats current in code
  base_station_wrong = which(endsWith(var_names, "base_station_ID"))
  if (length(base_station_wrong) > 0) {
    #change var names everywhere
    warning("State variable '<level>_base_station_ID' should be '<level>_basestation_ID' in RHESSys 7.1+")
    fix = FALSE
    if (fix) {
      var_names[base_station_wrong] = gsub("base_station_ID", "basestation_ID", var_names[base_station_wrong])
      template_clean[[base_station_wrong]][1] = gsub("base_station_ID", "basestation_ID", template_clean[[base_station_wrong]][1])
    }
  }
  basestation_ID_index = which(endsWith(var_names, "base_station_ID") | endsWith(var_names, "basestation_ID"))

  # get n basestations , check that strata are the same
  n_base = sapply(template_clean[n_basestations_index],FUN = function(x) as.integer(unique(x[3:length(x)])))
  if (length(n_base[[5]]) > 1) {stop(noquote("Canopy Strata n_basestations are inconsistent"))}
  for (i in seq_along(n_basestations_index)) {
    if (n_base[i] > 0) {
      if (!any((n_basestations_index[i] + 1) %in% basestation_ID_index)) {
        stop(noquote(paste("Missing basestation_ID on line", n_basestations_index[i] + 1,
                           ", ID is required since previous n_basestations is > 0, please fix in template")))
      }
    } else if (n_base[i] == 0) {
      if (any((n_basestations_index[i] + 1) %in% basestation_ID_index)) {
        stop(noquote(paste("Basestation_ID is present on line", n_basestations_index[i] + 1,
                           " while previous n_basestations is 0, either remove basestation_ID or modify n_basestaions to be > 0")))
      }
    }
  } # end loop through n_basestations

  # -------------------- Process Template + Maps --------------------

  # Build list based on operations called for by template
  statevars = vector("list",length(template_clean))

  for (i in var_index) {
    #level_agg = as.list(data.frame(levels[, 1:sum(i > level_index)]))
    if (nrow(levels) == 1) {
      level_agg = unname(split(levels[,i > level_index],f = seq_along(levels[,i > level_index])))
    } else {
      level_agg = as.list(data.frame(levels[,i > level_index]))
    }
    if (i > level_index[6]) {
      strata = 1:template_clean[[level_index[6]]][3] # for stratum level of template
    } else{
      strata = 1
    }
    # some error check, line by line
    if (template_clean[[i]][2] %in% c("value", "dvalue")) {
      if (suppressWarnings(all(is.na(as.numeric(template_clean[[i]][3]))))) {
        stop(noquote(paste0("\"",template_clean[[i]][3],"\" on template line ",i," is not a valid value.")))
      }
      if (length(strata) == 2) {
        if (suppressWarnings(all(is.na(as.numeric(template_clean[[i]][4]))))) {
          stop(noquote(paste0("\"",template_clean[[i]][4],"\" on template line ",i," is not a valid value.")))
        }
      }
    }
    strata_values = 2
    if (length(template_clean[[i]]) != 2 + length(strata) & template_clean[[i]][2] %in% c("value", "dvalue", "aver", "mode")) {
      if (length(template_clean[[i]]) == 2) {
        stop(noquote(paste0("Only 2 elements recognized ontemplate line ", i, ", expected format is <var name> <function> <value>")))
      } else if (length(template_clean[[i]]) == 3 & length(strata) == 2) {
        #warning("Using value '", template_clean[[i]][3], "' on template line ", i, " for both canopy strata")
        strata_values = 1
      }
    }

    s2 = 0
    for (s in strata) {
      if (s == 2 & strata_values == 1) {
        s2 = 1
      }
      if (template_clean[[i]][2] == "value") { # value (numeric)
        statevars[[i]][[s]] = as.double(template_clean[[i]][2 + s - s2])
      } else if (template_clean[[i]][2] == "dvalue") { #integer value
        statevars[[i]][[s]] = as.integer(template_clean[[i]][2 + s - s2])
      } else if (template_clean[[i]][2] == "char") { # character
        statevars[[i]][[s]] = as.character(template_clean[[i]][2 + s - s2])
      } else if (template_clean[[i]][2] == "aver") { #average
        maptmp = as.vector(t(map_df[template_clean[[i]][2 + s - s2]]))
        statevars[[i]][[s]] = aggregate(maptmp, by = level_agg, FUN = "mean")
      } else if (template_clean[[i]][2] == "mode") { #mode
        maptmp = as.vector(t(map_df[template_clean[[i]][2 + s - s2]]))
        statevars[[i]][[s]] = aggregate( maptmp, by = level_agg, FUN = mode_fun)
      } else if (template_clean[[i]][2] == "eqn") { # only for horizons old version -- use normal mean in future
        maptmp = as.vector(t(map_df[template_clean[[i]][5]]))
        statevars[[i]][[s]] = aggregate(maptmp, by = level_agg, FUN = "mean")
        statevars[[i]][[s]][, "x"] = statevars[[i]][[s]][, "x"] * as.numeric(template_clean[[i]][3])
      } else if (template_clean[[i]][2] == "spavg") { #spherical average
        maptmp = as.vector(t(map_df[template_clean[[i]][3]]))
        rad = (maptmp * pi) / (180) #convert to radians
        sin_avg = aggregate(sin(rad), by = level_agg, FUN = "mean") #avg sin
        cos_avg = aggregate(cos(rad), by = level_agg, FUN = "mean") #avg cos
        aspect_rad = atan2(sin_avg[, "x"], cos_avg[, "x"]) # sin and cos to tan
        aspect_deg = (aspect_rad * 180) / (pi) #rad to deg
        for (a in 1:length(aspect_deg)) {
          if (aspect_deg[a] < 0) {
            aspect_deg[a] = 360 + aspect_deg[a]
          }
        }
        statevars[[i]][[s]] = cos_avg
        statevars[[i]][[s]][, "x"] = aspect_deg
      } else if (template_clean[[i]][2] == "area") { #only for state var area
        statevars[[i]][[s]] = aggregate(cellarea, by = level_agg, FUN = "sum")
      } else {
        print(paste("Unexpected 2nd element on line", i))
      }
    }
  }

  # -------------------- Write World File --------------------

  # functions for replacing lapplys
  # basin
  bfun = function(i) {
    if (length(statevars[[i]][[1]]) > 1) {
      var = statevars[[i]][[1]][statevars[[i]][[1]][2] == b ,"x"]
    } else {var = statevars[[i]][[1]]}
    varname = template_clean[[i]][1]
    return(paste("\t",format(var),"\t\t\t",varname,"\n",sep = ""))
  }
  #hillslope
  hfun = function(i) {
    if (length(statevars[[i]][[1]]) > 1) {
      var = statevars[[i]][[1]][statevars[[i]][[1]][2] == b & statevars[[i]][[1]][3] == h ,"x"]
    } else {var = statevars[[i]][[1]]}
    varname = template_clean[[i]][1]
    return(paste("\t\t",format(var),"\t\t\t",varname,"\n",sep = ""))
  }
  #zone
  zfun = function(i) {
    if (length(statevars[[i]][[1]]) > 1) {
      var = statevars[[i]][[1]][statevars[[i]][[1]][2] == b & statevars[[i]][[1]][3] == h & statevars[[i]][[1]][4] == z ,"x"]
    } else {var = statevars[[i]][[1]]}
    varname = template_clean[[i]][1]
    return(paste("\t\t\t",format(var),"\t\t\t",varname,"\n",sep = ""))
  }
  #patch
  pfun = function(i) {

  }
  #stratum
  sfun = function(i) {

  }


  print("Writing worldfile",quote = FALSE)
  stratum = 1:template_clean[[level_index[6]]][3] # count of stratum

  # ----- Progress Bar -----
  # Iterates at hillslope level, shouldnt slow code too much
  progress = 0
  pb = txtProgressBar(min = 0, max = 1,style = 3)
  setTxtProgressBar(pb,0)

  # create/open connection
  wcon = file(worldfile,open = "wt")

  # ----- World -----
  # No state variables at world level
  world = unique(levels$w)
  writeChar(paste(world,"\t\t\t","world_ID\n",sep = ""),con = wcon, eos = NULL)
  writeChar(paste(length(unique(levels$b)),"\t\t\t","num_basins\n",sep = ""),con = wcon,eos = NULL)
  basin = unique(levels$b)

  # ----- Basin -----
  for (b in basin) {
    writeChar(paste("\t",b,"\t\t\t","basin_ID\n",sep = ""),con = wcon,eos = NULL)

    # for (i in (level_index[2] + 1):(level_index[3] - 1)) {
    #   if (length(statevars[[i]][[1]]) > 1) {
    #     var = statevars[[i]][[1]][statevars[[i]][[1]][2] == b ,"x"]
    #   } else {var = statevars[[i]][[1]]}
    #   varname = template_clean[[i]][1]
    #   writeChar(paste("\t",format(var),"\t\t\t",varname,"\n",sep = ""),con = wcon,eos = NULL)
    # }
    bout = unlist(lapply((level_index[2] + 1):(level_index[3] - 1), FUN = bfun))
    writeChar(bout,con = wcon,eos = NULL)

    hillslopes = unique(levels[levels$b == b, "h"])
    writeChar(paste("\t",length(hillslopes),"\t\t\t","num_hillslopes\n",sep = ""),con = wcon,eos = NULL)

    # ----- Hillslope -----
    for (h in hillslopes) {
      # Iterate progress bar
      progress = progress + 1
      setTxtProgressBar(pb,progress/length(unique(levels[,3])))

      writeChar(paste("\t\t",h,"\t\t\t","hillslope_ID\n",sep = ""),con = wcon,eos = NULL)

      # for (i in (level_index[3] + 1):(level_index[4] - 1)) {
      #   if (length(statevars[[i]][[1]]) > 1) {
      #     var = statevars[[i]][[1]][statevars[[i]][[1]][2] == b & statevars[[i]][[1]][3] == h ,"x"]
      #   } else {var = statevars[[i]][[1]]}
      #   varname = template_clean[[i]][1]
      #   writeChar(paste("\t\t",format(var),"\t\t\t",varname,"\n",sep = ""),con = wcon,eos = NULL)
      # }
      hout = unlist(lapply((level_index[3] + 1):(level_index[4] - 1), FUN = hfun))
      writeChar(hout,con = wcon,eos = NULL)

      zones = unique(levels[levels$h == h & levels$b == b, "z"])
      writeChar(paste("\t\t",length(zones),"\t\t\t","num_zones\n",sep = ""),con = wcon,eos = NULL)

      # ----- Zone -----
      for (z in zones) {
        writeChar(paste("\t\t\t",z,"\t\t\t","zone_ID\n",sep = ""),con = wcon,eos = NULL)

        # for (i in (level_index[4] + 1):(level_index[5] - 1)) {
        #   if (length(statevars[[i]][[1]]) > 1) {
        #     var = statevars[[i]][[1]][statevars[[i]][[1]][2] == b & statevars[[i]][[1]][3] == h & statevars[[i]][[1]][4] == z ,"x"]
        #   } else {var = statevars[[i]][[1]]}
        #   varname = template_clean[[i]][1]
        #   writeChar(paste("\t\t\t",format(var),"\t\t\t",varname,"\n",sep = ""),con = wcon,eos = NULL)
        # }
        zout = unlist(lapply((level_index[4] + 1):(level_index[5] - 1), FUN = zfun))
        writeChar(zout,con = wcon,eos = NULL)

        #---------- Start multiscale (aspatial) patches and stratum ----------
        if (asp_check) {
          patches = unique(levels[levels$z == z & levels$h == h & levels$b == b, "p"])
          asp_ct = sapply(rulevars, FUN = function(x) ncol(x[[1]]) - 1)
          if (length(patches) == 1 & length(asp_ct) == 1){
            total_patches = length(patches) * asp_ct
          } else {
            total_patches = sum(asp_ct[unique(levels[levels$z == z & levels$h == h & levels$b == b,])[,"a"]])
          }

          writeChar(paste("\t\t\t",total_patches,"\t\t\t","num_patches\n",sep = ""),con = wcon,eos = NULL)

          # ----- Patches (spatial) -----
          for (p in patches) {
            ruleid = paste0("rule_", unique(levels[(levels$p == p & levels$z == z & levels$h == h & levels$b == b), "a"]))

            if (length(ruleid) != 1) {stop("Multiple rule ids found for patch: ",p)}
            asp_index = 1:(length(rulevars[[(ruleid)]]$patch_level_vars[1,]) - 1)

            # ----- Patches (non-spatial) -----
            for (asp in asp_index) {
              pnum = (p*100) + asp # adjust patch numbers here - adds two 0's, ie: patch 1 becomes patches 101, 102, etc.

              writeChar(c(paste("\t\t\t\t",pnum,"\t\t\t","patch_ID\n",sep = ""),
                          paste("\t\t\t\t",p,"\t\t\t","family_ID\n",sep = "")),
                        con = wcon,eos = NULL)
              #writeChar(paste("\t\t\t\t",p,"\t\t\t","family_ID\n",sep = ""),con = wcon,eos = NULL)

              asp_p_vars = which(!rulevars[[ruleid]]$patch_level_vars[,1] %in% var_names[var_index]) # get vars from aspatial not included in template

              for (i in asp_p_vars) {
                # trying this without the asnumeric so I can add chars to worldfile
                #var = as.numeric(rulevars[[ruleid]]$patch_level_vars[i,asp + 1])
                var = rulevars[[ruleid]]$patch_level_vars[i,asp + 1]
                varname = rulevars[[ruleid]]$patch_level_vars[i,1]
                if (is.na(var)) {
                  stop(paste(varname,"cannot be NA since a default isn't specified in the template, please set explicitly in your rules file."))
                }
                writeChar(paste("\t\t\t\t",format(var),"\t\t\t",varname,"\n",sep = ""),con = wcon,eos = NULL)
              }

              for (i in (level_index[5] + 1):(level_index[6] - 1)) { #iterate through template-based state variables
                if (length(statevars[[i]][[1]]) > 1) {
                  var = statevars[[i]][[1]][statevars[[i]][[1]][2] == b & statevars[[i]][[1]][3] == h & statevars[[i]][[1]][4] == z & statevars[[i]][[1]][5] == p ,"x"]
                } else {var = statevars[[i]][[1]]}
                varname = template_clean[[i]][1]
                if (varname %in% rulevars[[ruleid]]$patch_level_vars[,1]) { # if variable is in rulevars, replace with rulevars version
                  if (!is.na(rulevars[[ruleid]]$patch_level_vars[rulevars[[ruleid]]$patch_level_vars[,1] == varname, asp + 1])) {
                    var = as.numeric(rulevars[[ruleid]]$patch_level_vars[rulevars[[ruleid]]$patch_level_vars[,1] == varname, asp + 1])
                  }
                }
                if (varname == "area") { # variable is area, adjust for pct_family_area
                  var = var * as.numeric(rulevars[[ruleid]]$patch_level_vars[rulevars[[ruleid]]$patch_level_vars[,1] == "pct_family_area",asp + 1])
                }
                writeChar(paste("\t\t\t\t",format(var),"\t\t\t",varname,"\n",sep = ""),con = wcon,eos = NULL)
              }

              asp_strata_ct = length(rulevars[[ruleid]]$strata_level_vars[[asp]][1,]) - 1
              strata_ct = asp_strata_ct

              writeChar(paste("\t\t\t\t",strata_ct,"\t\t\t","num_stratum\n",sep = ""),con = wcon,eos = NULL)

              # ----- Canopy Strata (for non-spatial patches) -----
              for (s in 1:strata_ct) {

                # Adds unique IDs for strataum intialized w/ same map as patches - appends 1 or 2 to the patch ID, ie patch 30 would have stratum 301 and 302
                if (unique_strata_ID) {
                  stratum_ID = pnum * 10 + s
                } else {
                  stratum_ID = unique(levels[levels[,5] == p & levels[,4] == z & levels[,3] == h & levels[,2] == b, 6])
                }
                writeChar(paste("\t\t\t\t\t",stratum_ID,"\t\t\t","canopy_strata_ID\n",sep = ""),con = wcon,eos = NULL)

                # if template has 1 strata and rules have 2 - replicate existing values if missing
                # if template has 2 and asp rules only has 1 strata, missing values will use only the 1st strata of the template
                if (length(stratum) == 1 & asp_strata_ct == 2 & s == 2) {s2 = 1} else {s2 = s}

                asp_s_vars = which(!rulevars[[ruleid]]$strata_level_vars[[asp]][,1] %in% var_names[var_index]) # get vars from aspatial not included in template
                for (i in asp_s_vars) {
                  var = as.numeric(rulevars[[ruleid]]$strata_level_vars[[asp]][i, s + 1])
                  varname = rulevars[[ruleid]]$strata_level_vars[[asp]][i, 1]
                  if (is.na(var)) {stop(paste(varname,"cannot be NA since a default isn't specified in the template, please set explicitly in your rules file."))}
                  writeChar(paste("\t\t\t\t",format(var),"\t\t\t",varname,"\n",sep = ""),con = wcon,eos = NULL)
                }

                for (i in (level_index[6] + 1):length(template_clean)) { # go through srata vars normally
                  if (length(statevars[[i]][[s2]]) > 1) { # its a map
                    var = statevars[[i]][[s2]][statevars[[i]][[s2]][2] == b & statevars[[i]][[s2]][3] == h & statevars[[i]][[s2]][4] == z & statevars[[i]][[s2]][5] == p ,"x"]
                  } else {var = statevars[[i]][[s2]]} # its a value
                  varname = template_clean[[i]][1]

                  if (varname %in% rulevars[[ruleid]]$strata_level_vars[[asp]][,1]) { # if variable is in rulevars, replace with rulevars version
                    if (!is.na(rulevars[[ruleid]]$strata_level_vars[[asp]][rulevars[[ruleid]]$strata_level_vars[[asp]][,1] == varname, s + 1])) { # make sure not NA
                      var = as.numeric(rulevars[[ruleid]]$strata_level_vars[[asp]][rulevars[[ruleid]]$strata_level_vars[[asp]][,1] == varname, s + 1])
                    }
                  }
                  writeChar(paste("\t\t\t\t\t",format(var),"\t\t\t",varname,"\n",sep = ""),con = wcon,eos = NULL)
                }
              }
            }
          } # end aspatial patches + stratum

          # ---------- Standard Patches + Stratum ----------
        } else {

          patches = unique(levels[levels[,4] == z & levels[,3] == h & levels[,2] == b, 5])
          writeChar(paste("\t\t\t",length(patches),"\t\t\t","num_patches\n",sep = ""),con = wcon,eos = NULL)

          # ----- Patches -----
          for (p in patches) {
            writeChar(paste("\t\t\t\t",p,"\t\t\t","patch_ID\n",sep = ""),con = wcon,eos = NULL)

            for (i in (level_index[5] + 1):(level_index[6] - 1)) {
              if (length(statevars[[i]][[1]]) > 1) {
                var = statevars[[i]][[1]][statevars[[i]][[1]][2] == b & statevars[[i]][[1]][3] == h & statevars[[i]][[1]][4] == z & statevars[[i]][[1]][5] == p ,"x"]
              } else {var = statevars[[i]][[1]]}
              varname = template_clean[[i]][1]
              writeChar(paste("\t\t\t\t",format(var),"\t\t\t",varname,"\n",sep = ""),con = wcon,eos = NULL)
            }
            writeChar(paste("\t\t\t\t",length(stratum),"\t\t\t","num_stratum\n",sep = ""),con = wcon,eos = NULL)

            # ----- Canopy Strata -----
            for (s in stratum) {

              # Adds unique IDs for strataum intialized w/ same map as patches - appends 1 or 2 to the patch ID, ie patch 30 would have stratum 301 and 302
              if (unique_strata_ID) {
                stratum_ID = unique(levels[levels[,5] == p & levels[,4] == z & levels[,3] == h & levels[,2] == b, 6]) * 10 + s
              } else {
                stratum_ID = unique(levels[levels[,5] == p & levels[,4] == z & levels[,3] == h & levels[,2] == b, 6])
              }
              writeChar(paste("\t\t\t\t\t",stratum_ID,"\t\t\t","canopy_strata_ID\n",sep = ""),con = wcon,eos = NULL)

              for (i in (level_index[6] + 1):length(template_clean)) {
                if (length(statevars[[i]][[s]]) > 1) { # if is a map
                  var = statevars[[i]][[s]][statevars[[i]][[s]][2] == b & statevars[[i]][[s]][3] == h & statevars[[i]][[s]][4] == z & statevars[[i]][[s]][5] == p ,"x"]
                } else {var = statevars[[i]][[s]]}
                varname = template_clean[[i]][1]
                writeChar(paste("\t\t\t\t\t",format(var),"\t\t\t",varname,"\n",sep = ""),con = wcon,eos = NULL)
              }

            } # end spatial statum
          }# end spatial patch
        } # end standard patches + stratum
      } # end zone
    } # end hillslope
  } # end basin

  close(wcon) # close file connection
  close(pb) # end progress bar connection

  print(paste("Created worldfile:",worldfile),quote = FALSE)

  # ---------------------- Write Header ----------------------
  if (header) {
    headfile = paste(substr(worldfile,0,(nchar(worldfile) - 5)),"hdr",sep = "")
    write(head,file = headfile)
    print(paste("Created header file:",headfile),quote = FALSE)
  }

  # ---------------------- Output for use in CreateFlownet ----------------------
  cfmaps = rbind(map_info,
                 c("cell_length",cell_len),
                 c("streams","none"), c("roads","none"), c("impervious","none"),c("roofs","none"))

  if (!"slope" %in% map_info[,1]) {
    slope = NULL
    for (i in 1:length(which(var_names == "slope"))) {
      slope = c(slope, template_clean[[which(var_names == "slope")[i]]][3])
    }
    cfmaps = rbind(cfmaps, c("slope", mean(as.numeric(slope))))
  }
  if (asp_check) {
    if (!"asp_rule" %in% cfmaps[,1]) {
      cfmaps = rbind(cfmaps, c("asp_rule", mean(as.numeric(template_clean[[which(var_names == "asp_rule")]][3]))))
    }
  }

  world_gen_out = list(cfmaps,rulevars)

  return(world_gen_out)

} # end function
