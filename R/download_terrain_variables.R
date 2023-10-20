#' Download terrain (DEM and NLCD) variables for U.S.
#'
#' This function will download a National Elevation Dataset (NED) DEM or
#' National Land Cover Database (NLCD) raster for an area of interest, reproject
#' the raster, and adjust spatial resolution.
#'
#' @param bbox A spatraster that defines the Area of Interest. Coordinates
#'   should be in long/lat. Needed functionality: a bbox vector in the form of
#'   c(........).
#' @param proj_epsg The UTM EPSG for reprojecting the terrain raster.
#' @param data_source Source of data. Current option is "FedData".
#' @param variable Name of the desired variable. Current options include "dem",
#'   and "nlcd".
#' @param label The name of the study area.
#' @param nlcd_year Year of nlcd data. Acceptable values are 2019 (default),
#'   2016, 2011, 2008, 2006, 2004, and 2001.
#' @param res Desired resolution in meters.
#' @param res_example An spatraster with desired resolution for the downloaded
#'   data to be resampled to. In many cases, this will be same raster as bbox.
#'   If both `res` and `res_example` are not null, `res_example` takes
#'   precedent.
#' @param output_file Path and file name of raster to be saved.
#'
#' @author Ryan Bart
#'
#' @export

download_terrain_variables <- function(bbox,
                                       proj_epsg,
                                       data_source,
                                       variable,
                                       label,
                                       nlcd_year = NULL,
                                       res = NULL,
                                       res_example = NULL,
                                       output_file){

  # -----------------
  # Process bbox

  # Todo: Currently only accepts a raster file

  # -----------------
  # Download data

  if (data_source == "FedData"){
    if (variable == "dem"){
      terrain_raster <- FedData::get_ned(template = bbox,
                                         label = label,
                                         force.redo = TRUE)
      method <- "bilinear"
    }
    if (variable == "nlcd"){
      terrain_raster <- FedData::get_nlcd(template = bbox,
                                          label = label,
                                          year = nlcd_year,
                                          force.redo = TRUE)
      method <- "near"
    }
  }

  # -----------------
  # Reproject

  terrain_raster_reproject <- terra::project(terrain_raster, y = terra::crs(paste0("epsg:", proj_epsg)))

  # -----------------
  # Change resolution

  # Resample function in Terra requires an example of the raster with the
  # correct new resolution. If 'res_example' is available, we use it. Otherwise,
  # we create an empty 'res_example' raster with the new resolution. Note that
  # the terra::res function keeps the xmin and ymin extents the same, but
  # slightly adjusts the xmax and ymax extents to permit the new resolution. We
  # then resample from the original raster to the 'res_example' raster.

  # Create raster with new resolution
  if (!is.null(res_example)){
    terrain_raster_empty <- res_example
  } else {
    terrain_raster_empty <- terrain_raster_reproject
    terra::res(terrain_raster_empty) <- res
  }

  # Resample
  terrain_raster_res <- terra::resample(x = terrain_raster_reproject,
                                        y = terrain_raster_empty,
                                        method = method)

  print(paste("Number of cells before reprojection:", terra::ncell(terrain_raster_reproject)))
  print(paste("Number of cells after reprojection:", terra::ncell(terrain_raster_res)))

  # -----------------
  # Write output

  terra::writeRaster(terrain_raster_res, filename = output_file, overwrite=TRUE)

  return(terrain_raster_res)
}


