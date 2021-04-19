# utils

# this is probably a bad idea
init_defaults = function() {
  missing  = formals(RHESSysPreprocess)[!formalArgs(RHESSysPreprocess) %in% ls(.GlobalEnv)]
  for (i in seq_along(missing)) {
    assign(x = names(missing[i]), value = unname(unlist(missing[i])))
  }

}


# run this is preprocess isn't working to check out if anything stands out with the maps
# i'll try to add checks to this as needed.
# debug_spatial = function(template, typepars, streams) {
#
#   template = template_read(template = template)
#   maps = as.data.frame(unique(template[[5]]))
#   maps = rbind(maps, c("streams", streams))
#
#   map_paths = file.path(typepars, maps$Map)
#
#   maps$Exists = sapply(map_paths, file.exists)
#
#   library(raster)
#   # going to assume this might not work, tho this is how they are read normally
#   #map_stack = raster::stack(x = map_paths)
#   map_data = list()
#   for (i in seq_along(map_paths)) {
#     map_data[[i]] = raster(map_paths[i])
#
#     maps$extent = paste(map_data[[i]]@extent[1:4])
#     maps$cellsize = res(map_data[[i]])
#
#     maps$ncell = ncell(map_data[[i]])
#     maps$NAs = sum(is.na(values(map_data[[i]])))
#     maps$NULLs = sum(is.null(map_data[[i]]))
#     maps$proj = paste(crs(map_data[[i]]@crs))
#
#   }
#
# }
