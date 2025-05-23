#' template_read
#'
#' Reads in a RHESSys template file, produces a clean list, variable name vector, indices of levels and variables
#' @param template template name and path to be read in
#' @return Returns a list containing (in order): template in list form, variable name vector, index of levels,
#' index of variables, full table of maps and associated variables
#' @author Will Burke
#' @export

template_read = function(template){

  options(scipen = 999) # no scientific notation

  # ---------- Read in template ----------
  con = file(template, open = "r") # commect to file
  read = readLines(con) # read file, default reads entire file, line by line
  close(con)

  trim = trimws(read)
  trimlines = trim[trim != ""]
  if (min(which(startsWith(trimlines, "_"))) > 1) {
    head = trimlines[1:(min(which(startsWith(trimlines, "_"))) - 1)]
  } else {
    head = "No header"
  }
  template_clean = strsplit(trimlines,"[ \t]+") # remove whitespaces, split strings by spaces or tabs

  var_names = unlist(lapply(template_clean,"[[",1)) #all names of state variables in template

  level_index = which(startsWith(var_names, "_")) # find lines that start w/ "_", get row nums of levels
  level_maps = lapply(template_clean[level_index],"[",2)# level map names, for use in GRASS
  var_index = level_index[2]:length(template_clean)
  var_index = var_index[!var_index %in% level_index] #make index for template, excluding def files and levels

  # Find all maps - in addiiion to level maps
  maps_all = vector()
  maps_index = vector()
  for (i in var_index) {
    if (template_clean[[i]][2] != "char" && suppressWarnings(all(is.na(as.numeric(template_clean[[i]][3]))))  & length(template_clean[[i]]) != 2) {
      maps_all = c(maps_all, template_clean[[i]][3])
      maps_index = c(maps_index, i)
    }
    if (i > level_index[6] & suppressWarnings(all(is.na(as.numeric( template_clean[[i]][4]))))  & length(template_clean[[i]]) == 4) {
      maps_all = c(maps_all, template_clean[[i]][4])
      maps_index = c(maps_index, i)
    }
    if (length(template_clean[[i]]) == 5 ) { # this should just be for horizons
      maps_all = c(maps_all, template_clean[[i]][5])
      maps_index = c(maps_index, i)
    }
  }
  
  maps_index = maps_index[!is.na(maps_index)] # index of rows w/ maps
  map_names = sapply(template_clean[maps_index], function(x) x[1])
  if (is.list(map_names)) {
    map_names = unlist(map_names)
  }

  # IF STRATA HAS A MAP INSTEAD OF A NUMBER, READ IT TO USE TO DYNAMICALLY SET STRATA NUM
  s_ind = which(var_names == "_canopy_strata")
  if (suppressWarnings(all(is.na(as.numeric(template_clean[[s_ind]][3]))))) {
    maps_all = c(maps_all, template_clean[[s_ind]][3])
    maps_index = c(maps_index, s_ind)
    map_names = c(map_names, "strata_count")
  }

  map_info = cbind(c("world","basin","hillslope","zone","patch","strata", map_names),c(unlist(level_maps),maps_all[!is.na(maps_all)]))
  colnames(map_info) = c("MapName","Map")
  #map_info = unique(map_info)

  template_list = list(template_clean,var_names,level_index,var_index,map_info,head)
  return(template_list)

}
