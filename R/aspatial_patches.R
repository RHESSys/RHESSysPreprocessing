#' Aspatial Patch Generation
#' @param asprules Input aspatial rules file
#' @param asp_mapdata map data or value, indicating the rule IDs being used
#' @author Will Burke

aspatial_patches = function(asprules,asp_mapdata) {

  # some functions
  # splits a vector up at a specific character
  splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

  # same as above but removes the character that it splits at and convets to numeric
  splitAt2 <- function(x, pos) {
    f = cumsum(seq_along(x) %in% pos)
    f[pos] = NA
    lapply(unname(split(x, f)),as.numeric)
  }

  # ---------- Read rules file ----------
  con = file(asprules, open = "r") # commect to file
  readrules = readLines(con) # read file, default reads entire file, line by line
  close(con)

  # ---------- Parse into list of all rules ----------
  rules_in_trim = trimws(readrules)
  rules_in_trim = rules_in_trim[!rules_in_trim == ""]
  #rules_head = rules_in_trim[0:(min(which(startsWith(rules_in_trim,"ID")))-1)] # separate header
  rules_header = rules_in_trim[which(startsWith(rules_in_trim,"#"))] # this is for later use potentially, not being kept currently
  #rules_in_trim = rules_in_trim[min(which(startsWith(rules_in_trim,"ID"))):length(rules_in_trim)]
  rules_in_trim = rules_in_trim[-which(startsWith(rules_in_trim,"#"))]
  id_ind = which(startsWith(rules_in_trim,"ID")) # index IDs
  patch_ind = c(which(startsWith(rules_in_trim,"_patch")), which(startsWith(rules_in_trim,"_Patch"))) # index patches
  strata_ind = c(which(startsWith(rules_in_trim,"_stratum")), which(startsWith(rules_in_trim,"_canopy_strata"))) # index strata
  rule_split = strsplit(rules_in_trim,"[ \t]+") # split strings at tabs and spaces

  id_split = splitAt(rule_split,c(id_ind,patch_ind,strata_ind)) # split rules by IDs, patches, and strata
  rule_list = splitAt(id_split,seq(1,length(id_split),3)) # split again by rule IDs -
  # list structure: top level list -id info (ID and subpatch count),patch,strata. second -statevars and values for each
  names(rule_list) = paste("rule_",sapply(lapply(rule_list,"[[",1),"[[",1)[2,],sep = "")

  # ---------- Build output data object(s) of patch and strata statevars ----------

  # STRUCTURE CHANGE
  # List of rule IDs
  # Each rule is a list, containing:
  # patch data frame, rows are attributes, cols are subpatches
  # strata list,containing
  # strata data frames (num = patch ct) - rows are statevars, cols are strata

  # ---------- build rulevars based on rules and map data ----------
  map_ids = unique(asp_mapdata) # get rule IDs from map/input
  map_id_tags = paste("rule_",map_ids,sep = "") # all map IDs concated w tags for referencing/reading in code

  asp_vars = as.list(rep(0,length(map_ids))) # highest level list of the different rules
  # strata_index = as.list(rep(0,length(map_ids))) # get rid of in this version i think
  names(asp_vars) = map_id_tags
  # names(strata_index) = map_id_tags

  for (id_tag in map_id_tags) { # iterate through rule IDs

    subpatch_ct = as.numeric(rule_list[[id_tag]][[1]][[which(sapply(rule_list[[id_tag]][[1]],"[[",1) == "subpatch_count")]][2])
    patch_var_list = rule_list[[id_tag]][[2]] # get patch vars
    if (length(patch_var_list) == 1) { # if no vars, just header, make NULL
      patch_var_list = NULL
    } else {
      patch_var_list = patch_var_list[-which(sapply(patch_var_list, "[[", 1) == "_patch")]
    }
    patch_df = as.data.frame(matrix(ncol = (subpatch_ct + 1), nrow = length(patch_var_list))) # data frame for sub patch state vars
    names(patch_df) = c("state_var", paste("patch_",1:subpatch_ct,sep = ""))
    patch_df$state_var = sapply(patch_var_list, "[[", 1) # add state var names - assumes first item is state var name - pretty safe assumption

    if (!is.null(patch_var_list)) {
      for (var in 1:length(patch_var_list)) { # go through every patch level variable

        if (patch_var_list[[var]][[2]] == "value" | patch_var_list[[var]][[2]] == "dvalue" | patch_var_list[[var]][[2]] == "char") { # check if second element is equation/modifier
          patch_vars = patch_var_list[[var]][3:length(patch_var_list[[var]])] # extract everything else
        } else {
          patch_vars = patch_var_list[[var]][2:length(patch_var_list[[var]])]
        }
        # ----- Parsing -----
        #num_index = which(suppressWarnings(!is.na(as.numeric(patch_vars)))) # positions of numbers
        var_index = which(!patch_vars %in% "|")

        if (length(var_index) > subpatch_ct) {
          stop(paste(id_tag,"patch variable",var,"contains more values than (aspatial) patches"))
        }
        if (length(var_index) == subpatch_ct | length(var_index) == 1) { # if it's the right number of values or 1 value
          if (patch_var_list[[var]][[2]] == "char") {
            patch_df[var,2:length(patch_df[var,])] = as.character(patch_vars[var_index])
          } else {
            patch_df[var,2:length(patch_df[var,])] = as.numeric(patch_vars[var_index])
          }
        } else if (length(var_index) < subpatch_ct) { # messier if using | to denote where to fill in w base template
          cat("Rules are messy, might not work")
          var_split = splitAt(patch_vars, which(patch_vars == "|"))
          patch_df[var,2] = as.numeric(var_split[[1]])
          for (i in 2:length(var_split)) {
            if (length(var_split[[i]]) == 1) {var_split[[i]][2] = NA}
          }
          patch_df[var,3:length(patch_df[var,])] = sapply(var_split[2:length(var_split)], "[[", 2)
        }

      }
    }

    # ----- Strata -----
    strata_var_list = rule_list[[id_tag]][[3]] # get strata vars

    # get strata count(s)
    num_index = which(suppressWarnings(!is.na(as.numeric(strata_var_list[[1]])))) # positions of numbers
    #var_index = which(!strata_var_list[[1]] %in% "|")

    if (length(num_index) == subpatch_ct) {
      strata_ct = as.integer(strata_var_list[[1]][num_index]) # strata counts for each subpatch
    } else if (length(num_index) == 1) {
      strata_ct = rep(as.integer(strata_var_list[[1]][2]),subpatch_ct) # one strata count, use for all subpatches
    }

    if (length(strata_var_list) == 1) { # if no vars, just header
      strata_var_list = NULL
    } else {
      strata_var_list = strata_var_list[-which(sapply(strata_var_list, "[[", 1) == "_canopy_strata")]
    }

    if (!is.null(strata_var_list)) {

      # make strata list
      strata_list = replicate(subpatch_ct,data.frame(),simplify = FALSE) # num of df = subpatch count
      names(strata_list) =  paste("patch_",1:subpatch_ct,sep = "") # this has to be done again later since the names get lost in lapply
      for (i in 1:subpatch_ct) {
        strata_df = as.data.frame(matrix(ncol = (strata_ct[i] + 1), nrow = length(strata_var_list))) # data frame for strata
        names(strata_df) = c("state_var", paste("canopy_strata_",c(1:strata_ct[i]),sep = ""))
        strata_df$state_var = sapply(strata_var_list, "[[", 1)
        strata_list[[i]] = strata_df
      }

      for (var in 1:length(strata_var_list)) { # go through strata state vars

        if (strata_var_list[[var]][[2]] == "value" | strata_var_list[[var]][[2]] == "dvalue") { # check if second element is equation/modifier
          strata_vars = strata_var_list[[var]][3:length(strata_var_list[[var]])] # extract everything else
        } else {
          strata_vars = strata_var_list[[var]][2:length(strata_var_list[[var]])]
        }

        # CHANGE THIS TO USE THE SPLITAT2 VALUES AND COUNT THOSE
        num_index = which(suppressWarnings(!is.na(as.numeric(strata_vars)))) # positions of numbers

        if (length(num_index) >  sum(strata_ct)) {
          stop(paste(id_tag,"strata variable",var,"contains too many values"))
        }

        if (length(num_index) == sum(strata_ct)) { # if it's the exact right number of values
          var_split = splitAt2(strata_vars, which(strata_vars == "|"))
          strata_list = lapply(seq_along(strata_list), FUN = function(x,y) {y[[x]][var,2:length(y[[x]][var,])] = var_split[[x]]; return(y[[x]])}, y = strata_list)

        } else if (length(num_index) != sum(strata_ct)) { # if it's NOT the right number of values - repeat values as needed
          var_split = splitAt2(strata_vars, which(strata_vars == "|"))
          strata_list = lapply(seq_along(strata_list), FUN = function(x,y) {
            if (length(var_split[[x]]) < strata_ct[x] & length(var_split[[x]]) == 1) {var_split[[x]] = rep(var_split[[x]],strata_ct[x])}
            y[[x]][var,2:length(y[[x]][var,])] = var_split[[x]]
            return(y[[x]])
          }
          , y = strata_list)
        }


      }
    }

    # ----- combine data frames into asp_vars list -----

    names(strata_list) =  paste("patch_",1:subpatch_ct,sep = "")

    asp_vars[[id_tag]] = list(patch_df, strata_list)
    names(asp_vars[[id_tag]]) = c("patch_level_vars", "strata_level_vars")

  } # end rule ID itr


  # ---------- Output ----------

  return(asp_vars)

  #lret = list(asp_vars,strata_index)
  #return(lret)

} #end function
