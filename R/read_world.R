#' read_world
#'
#' Reads a worldfile
#' @param worldfile Source worldfile
#' @param hill_col TRUE/FALSE if a column of only hillslope IDs should be added,
#' for the hillslope level and levels below (zone, patch, canopy_strata)
#' @param zone_col TRUE/FALSE if a column of only zone IDs should be added,
#' for the zone level and levels below (patch, canopy_strata)
#' @param patch_col TRUE/FALSE if a column of only patch IDs should be added,
#' for the patch level and levels below (canopy_strata)
#' @author Will Burke
#' @export

read_world = function(worldfile, hill_col = F, zone_col = F, patch_col = F) {

  # ---------- Parse Worldfile ----------
  # parsing the values as characters to retain the exact value/precision
  read_world = readLines(worldfile, warn = FALSE, encoding = "UTF-8")
  read_world = read_world[nchar(read_world) > 0]
  world =  strsplit(trimws(read_world), "\\s+")
  world = data.frame(matrix(unlist(world), nrow = length(world), byrow = T), stringsAsFactors = FALSE)
  names(world) = c("values","vars")

  # ---------- Find Levels----------
  index_all = which(world$vars == "world_ID" | world$vars == "basin_ID" | world$vars == "hillslope_ID" |
                      world$vars == "zone_ID" | world$vars == "patch_ID" | world$vars == "canopy_strata_ID")
  index_names = gsub("_ID", "", x = world$vars[index_all])
  index_max = c(index_all[2:length(index_all)] - 1, length(world$vars))
  world$level = unname(unlist(mapply(rep, index_names, (index_max - index_all) + 1 )))
  world$ID = unname(unlist(mapply(rep, world$values[index_all], (index_max - index_all) + 1 )))

  if (hill_col) {
    index_hill = which(world$vars == "hillslope_ID")
    if (length(index_hill) > 1) {
      index_hill_max = c(index_hill[2:length(index_hill)] - 1, length(world$vars))
    } else {
      index_hill_max = length(world$vars)
    }
    world$hillslope_ID = c(rep(NA, index_hill[1]-1) ,unname(unlist(mapply(rep, world$values[index_hill], (index_hill_max - index_hill) + 1 ))))
  }
  if (zone_col) {
    index_zone = which(world$vars == "zone_ID")
    if (length(index_zone) > 1) {
      index_zone_max = c(index_zone[2:length(index_zone)] - 1, length(world$vars))
    } else {
      index_zone_max = length(world$vars)
    }
    world$zone_ID = c(rep(NA, index_zone[1]-1) ,unname(unlist(mapply(rep, world$values[index_zone], (index_zone_max - index_zone) + 1 ))))
  }
  if (patch_col) {
    index_patch = which(world$vars == "patch_ID")
    if (length(index_patch) > 1) {
      index_patch_max = c(index_patch[2:length(index_patch)] - 1, length(world$vars))
    } else {
      index_patch_max = length(world$vars)
    }
    world$patch_ID = c(rep(NA, index_patch[1]-1) ,unname(unlist(mapply(rep, world$values[index_patch], (index_patch_max - index_patch) + 1 ))))
  }

  # get unique ID - useful for queries/parsing later
  world$unique_ID = unname(unlist(mapply(rep, c(1:length(index_names)), (index_max - index_all) + 1 )))

  return(world)
}
