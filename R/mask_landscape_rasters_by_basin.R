#' Mask landscape rasters by basin
#'
#' This function masks landscape rasters by the basin extent for use in
#' `RHESSysPreprocess.
#'
#' @param directory Path to directory containing raster inputs/outputs. Inputs should
#'   include rasters to be masked.
#' @param file_identifier Syntax in file name that identifies a raster for
#'   masking. The file_identifier is removed from the outout filename.
#' @param Option to return masked rasters as list of R objects in addition to
#'   exporting to file.
#'
#' @author Ryan Bart
#'
#' @export

mask_landscape_rasters_by_basin <- function(directory,
                                            file_identifier = "_landscape",
                                            return_rasters = FALSE){

  # Identify all landscape files in directory
  rasters_to_mask <- list.files(path = directory, pattern = file_identifier)
  # Generate output names without file identifier
  names_output <- sub(file_identifier, "", rasters_to_mask)

  # Import basin file
  basin <- terra::rast(file.path(directory, "basin.tif"))

  # Import files to be masked, mask them, and export them
  masked_rasters <- lapply(seq_along(rasters_to_mask), FUN = function(x){

    raster_to_mask <- terra::rast(file.path(directory, rasters_to_mask[x]))
    masked_raster <- terra::mask(raster_to_mask, mask = basin)
    terra::writeRaster(masked_raster, filename = file.path(directory, paste0(names_output[x], ".tif")), overwrite=TRUE)

    return(masked_raster)
  })

  if (return_rasters){
    return(masked_rasters)
  } else {
    return(NULL)
  }
}


