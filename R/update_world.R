#' update_world
#'
#' Read and modify an existing worldfile. Optimized somewhat (for R) but awk may still be faster.
#' @param worldfile Name and path of worldfile to be created.
#' @param vars State variable(s) from the worldfile to be modified
#' @param values Values to replace for `vars`, or coefficients to multiply by. N args must equal that of vars or be 1 (will be used for all vars).
#' or can be a list of values where number of list elements = n vars
#' @param level_names The level for which a given state variable will have it's value changed or replaced. Can be a single character string or vector
#' of character strings. Valid arguments are: world, basin, hillslope, zone, patch, canopy_strata.
#' @param level_IDs The IDs of the correstponding `level_names` for which a given state variable will have it's value changed or replaced. Set to NULL or "all"
#' to change a value across all of a given level (e.g. for all patches). This can be a vector same length as names, or a list, with IDs for each named level in names
#' @param veg_parm_ID Veg parm IDs to subset
#' @param out_file Destination file to write new worldfile
#' @param overwrite TRUE/FALSE if input worldfile should be overwritten
#' @author Will Burke
#'
#' @export

update_world = function(worldfile, out_file = NULL, vars, values, veg_parm_ID = NULL, level_names = NULL, level_IDs = NULL, overwrite = FALSE) {

  options(scipen = 999)

  # NOTES
  # - vdouble check all the list options work and check var lengths/numbers when needed

  # ---------- Check Aguments ----------
  # if (!exists(worldfile)) {stop(noquote("Input worldfile is required"))}
  if (!file.exists(worldfile)) {stop(noquote(paste0("No file found at", worldfile)))}
  if (is.null(out_file) & !overwrite) {stop(noquote("No destination file set by 'out_file' and 'overwrite' is FALSE"))}
  if (file.exists(out_file) & overwrite == FALSE) {stop(noquote(paste0("File '",out_file,"' already exists and 'overwrite' argument is FALSE")))}
  #if (length(vars) != length(values) & length(values) != 1 & !is.list(values)) {}

  # if (!is.list(values)) {
  #   if (!is.character(values)) {
  #     values = as.character(values)
  #   }
  # }

  # for (i in length(vars)) {
  #   if (level_names[i] == "all") {level_names[i] = c("world", "basin", "hillslope", "zone", "patch", "canopy_strata")}
  # }
  #
  # if (level_IDs == "all") {}

  # ---------- Parse Worldfile ----------
  # parsing the values as characters to retain the exact value/precision
  read_world = readLines(worldfile, warn = FALSE)
  read_world = read_world[nchar(read_world) > 0]
  world =  strsplit(trimws(read_world), "\\s+")
  world = data.frame(matrix(unlist(world), nrow=length(world), byrow=T),stringsAsFactors = FALSE)
  names(world) = c("values","vars")

  # ---------- Find Levels----------
  index_all = which(world[,2] == "world_ID" | world[,2] == "basin_ID" | world[,2] == "hillslope_ID" |
                      world[,2] == "zone_ID" | world[,2] == "patch_ID" | world[,2] == "canopy_strata_ID")
  index_names = gsub("_ID", "", x = world$vars[index_all])
  index_max = c(index_all[2:length(index_all)]-1, length(read_world))
  world$level = unname(unlist(mapply(rep,index_names, (index_max - index_all) + 1 )))
  world$ID = unname(unlist(mapply(rep, world$values[index_all], (index_max - index_all) + 1 )))

  world$unique = 1
  for (i in 2:length(world$unique)) {
    if (world$level[i] != world$level[i - 1] | world$ID[i] != world$ID[i - 1]) {
      world$unique[i] = world$unique[i - 1] + 1
    } else {
      world$unique[i] = world$unique[i - 1]
    }
  }

  # ---------- Find and Replace Vars ----------
  if (!is.null(level_names)) {
    if (length(level_names) == 1 & length(vars) != 1) {level_names = rep.int(level_names, length(vars))}
  }
  if (!is.null(level_IDs)) {
    if (length(level_IDs) == 1 & length(vars) != 1) {level_IDs = rep.int(level_IDs, length(vars))}
  }
  if (!is.null(veg_parm_ID)) {
    if (length(veg_parm_ID) == 1 & length(vars) != 1) {veg_parm_ID = rep.int(veg_parm_ID, length(vars))}
  }

  for (i in 1:length(vars)){

    if (!is.null(level_names[i])) {
      find_index = world$level == level_names[i]
      if (sum(find_index) == 0) {stop("no levels matched input level_name")}
    } else {
      find_index = rep(TRUE,length(world$level))
    }
    if (!is.null(level_IDs[i])) {
      find_index = find_index & world$ID == level_IDs[i]
      if (sum(find_index) == 0) {stop("no level IDs in the named levels matched input level_ID")}
    }
    if (!is.null(veg_parm_ID[i])) {
      veg_parm_unique = world$unique[world$vars == "veg_parm_ID" & world$values == veg_parm_ID[i]]
      find_index = find_index & world$unique %in% veg_parm_unique
      if (sum(find_index) == 0) {stop("no matched veg_parm_IDs")}
    }
    replace_index = which(world$vars == vars[i] & find_index)

    # if unique values for every instance of var to be replaces were given, do nothing, otherwise repeat to get enough replacement values
    current_value = world$values[replace_index]
    if (length(values[i]) != length(replace_index)) {
      new_value = rep(values[i], length(replace_index)/length(values[i]))
    } else {
      new_value = values[i]
    }

    if (is.character(new_value)) {
      if (any(startsWith(new_value,"*"))) {
        new_value = as.numeric(trimws(substr(new_value[startsWith(new_value,"*")],2,nchar(new_value[startsWith(new_value,"*")]))))
        new_value = new_value * as.numeric(current_value)
      } else {
        new_value = as.numeric(trimws(new_value))
      }
    }


    # generic sub/gsub
    #sub("\\d+[[:blank:]]",XXX"\t", read_world)

    read_world[replace_index] = unname(mapply(sub,paste0(current_value,"[[:blank:]]"),paste0(new_value,"\t"),read_world[replace_index]))

  }

  # ---------- Write file ----------
  writeLines(text = read_world,out_file)

  print(noquote(paste("Successfully wrote updated worldfile to",out_file)))

}
