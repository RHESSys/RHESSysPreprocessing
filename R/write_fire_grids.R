#' Write fire grids
#'
#' Writes ascii formatted, headerless, fire grid files for use with the RHESSys integrated WMFire model. Also outputs a info file
#' containing the grid dimensions and cell size.
#' @param name Basename for the output fire grids. This will be the fire grid prefix given to rhessys to specify the fire grid files.
#' @param template Input template file referencing the DEM, patch, zone, and hillslope spatial inputs.
#' @param map_dir Directory containing the input maps (referenced as 'typepars' elsewhere)
#' @param seq_patch_IDs TRUE/FALSE should sequential patch IDs be created (only needed if they were when worldfile was created)
#' @author Will Burke
#' @export

# name = "BC_h2"
# template = "Preprocessing/template/coupling_test.template"
# map_dir = "Preprocessing/BC_spatial/hillID_2/"

write_fire_grids = function(name, template, map_dir, seq_patch_IDs = FALSE) {

  template_in = template_read(template)
  maps = template_in[[5]]
  maps = maps[maps[, 1] %in% c("world", "basin", "hillslope", "zone", "patch", "z"),]
  maps = unique(maps)

  # could kinda skip this but this makes sure the same cropping/clipping happens the same as it does for the worldfile
  maps_in = GIS_read(
    maps_in = unique(maps[, 2]),
    map_dir = map_dir,
    map_info = maps,
    seq_patch_IDs = seq_patch_IDs,
    output_patch_map = FALSE
  )

  #maps_rast = methods::as(maps_in, "RasterStack")

  # write.asciigrid needs even cell sizes and projections can lead to very slight differences
  if (length(unique(terra::res(maps_in))) != 1) {
    terra::res(maps_in) = round(terra::res(maps_in), 5)
    if (length(unique(terra::res(maps_in))) != 1) {
      warning("Cell sizes are not square - using mean of cell sizes so write.asciigrid will work")
      terra::res(maps_in) = mean(terra::res(maps_in))
    }
  }

  files_in = unname(c(maps[maps[, 1] == "z", 2],
                      maps[maps[, 1] == "hillslope", 2],
                      maps[maps[, 1] == "zone", 2],
                      maps[maps[, 1] == "patch", 2]))
  file_types = c("dem", "hillslope", "zone", "patch")
  files_out = file.path(map_dir, file_types)

  stop("function broken -- need to replcae sp function write.asciigrid with terra or sf version. line 52 in write_fire_grids.R")

  # write_rast = function(X) {
  #   write.asciigrid(
  #     x = maps_in[files_in[X]],
  #     fname = files_out[X]
  #   )
  # }
  shh = lapply(seq_along(files_out), write_rast)

  linesin = lapply(files_out, readLines)
  shhh = file.remove(files_out)
  heads = lapply(linesin, "[", c(1:6))
  linesout = lapply(linesin, function(x) {
    x[7:length(x)]
  })
  write_files = file.path(map_dir, paste0(name, ".", file_types))
  shhhh = mapply(writeLines, linesout, write_files)
  cat(
    "Wrote fire grid files to map dir:",
    map_dir,
    "with levels appended as extensions (.dem, .hillslope, .zone, .patch)"
  )

  # write header(s) info
  writeLines(text = unlist(unique(heads)),
             con = file.path(map_dir, "grid_info.txt"))
  cat(
    "Wrote header info to:",
    file.path(map_dir, "grid_info.txt"),
    "duplicate entries indicate varying header info"
  )

}
