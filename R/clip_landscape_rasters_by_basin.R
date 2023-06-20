#' Clip landscape rasters by basin
#'
#' This function clips landscape rasters by the basin extent for use in
#' `RHESSysPreprocess.
#'
#' @param work_folder Path to folder containing inputs/outputs. Inputs should
#'   include rasters to be clipped.
#' @param file_identifier Syntax in file name that identifies a raster for
#'   clipping. The file_identifier is removed from the outout file name.
#' @param Option to return clipped rasters as list of R objects in addition to
#'   exporting to file.
#'
#' @author Ryan Bart
#'
#' @export

clip_landscape_rasters_by_basin <- function(work_folder,
                                            file_identifier = "_landscape",
                                            return_rasters = FALSE){

  # Identify all landscape files in work_folder
  rasters_to_clip <- list.files(path = work_folder, pattern = file_identifier)
  # Generate output names without file identifier
  names_output <- sub(file_identifier, "", rasters_to_clip)

  # Import basin file
  basin <- terra::rast(file.path(work_folder, "basin.tif"))

  # Import files to be clipped, clip them, and export them
  clipped_rasters <- lapply(seq_along(rasters_to_clip), FUN = function(x){

    raster_to_clip <- terra::rast(file.path(work_folder, rasters_to_clip[x]))
    clipped_raster <- terra::mask(raster_to_clip, mask = basin)
    terra::writeRaster(clipped_raster, filename = file.path(work_folder, names_output[x]), overwrite=TRUE)

    return(clipped_raster)
  })

  if (return_rasters){
    return(clipped_rasters)
  } else {
    return(NULL)
  }
}


