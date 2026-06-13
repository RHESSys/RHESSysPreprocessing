#' Make landscape patch raster for watershed processing
#'
#' This function generates a patch raster based on the method selected.
#'
#' @param directory Path to directory containing landscape rasters produced by
#'   'make_landscape_rasters' function.
#' @param patch_method Options for generating patch raster. Options include
#'   'simple' (default).
#'
#' @return Returns patch raster named 'patch_landscape.tif'
#'
#' @author Ryan Bart
#'
#' @section Note: The following sections describe the patch map options.
#'
#'   The 'simple' option generates a unique patch ID for each dem pixel with no
#'   grouping.
#'
#' @export

make_landscape_rasters_patch <- function(directory,
                                         patch_method = "simple"){

  # Check that directory exists. If it doesn't, then make it, if possible.
  if(!dir.exists(directory)){dir.create(directory)}

  print("***** Creating patch maps *****")

  # Simple patch raster
  if (patch_method == "simple"){
    dem <- terra::rast(dem)
    terra::values(dem) <- seq(1,terra::ncell(dem))
    terra::writeRaster(dem, filename = file.path(directory, "patch_landscape.tif"), overwrite=TRUE)
  }

  return(NULL)
}


