#' Make a basin raster
#'
#' This function generates a basin raster by 1) adjusting a pour point (aka
#' outlet) to the stream network, and 2) cutting out the corresponding basin.
#'
#' @param work_folder Path to folder containing inputs/outputs
#' @param pour_point_file Filename&extension (if located in work_folder), or
#'   path&filename&extension, of a shapefile containing a point designating the
#'   basin outlet. Projection of shapefile should be same as `streams` and
#'   `d8_pointer` This argument overrides `pour_point_longlat`.
#' @param pour_point_longlat Vector containing the longitude and latitude for a
#'   point designating the basin outlet. The coordinate will be reprojected to
#'   the `proj_epsg` projection and then a shapefile will be generated, which
#'   will subsequently be used as a direct input into Whitebox function. The new
#'   shapefile will be written to `work_folder`. This argument will be ignored
#'   if `pour_point_file` is not NULL.
#' @param proj_epsg EPSG of project that `pour_point_longlat` will be converted
#'   to. Should be a UTM zone.
#' @param streams Filename&extension (if located in work_folder), or
#'   path&filename&extension, of the streams raster to be inputted.
#' @param d8_pointer Filename&extension (if located in work_folder), or
#'   path&filename&extension, of the D8 pointer raster to be inputted.
#' @param snap_dist Maximum snap distance in map units (Meters for UTM).
#' @param save_map Option to save a map showing pour point adjustment on stream
#'   network.
#'
#' @author Ryan Bart
#'
#' @section Note: WhiteboxTools Open Core (https://www.whiteboxgeo.com/) must be
#'   installed on your computer to run this function. The R package whitebox is
#'   an R wrapper that interfaces with Whitebox.
#'
#'   Several improvements can be made to this function. 1) May be
#'   able to get rid of `proj_epsg` and take desired projection directly from
#'   `streams` and `d8_pointer`. 2) It would probably be better to generate
#'   shiny app or leaflet map to identify outlet.
#'
#'   Links to embedded functions
#'   https://www.whiteboxgeo.com/manual/wbt_book/available_tools/hydrological_analysis.html#JensonSnapPourPoints
#'   https://www.whiteboxgeo.com/manual/wbt_book/available_tools/geomorphometric_analysis.html#Watershed
#'
#' @export

make_basin_raster <- function(work_folder,
                              pour_point_file = NULL,
                              pour_point_longlat = NULL,
                              proj_epsg = NULL,
                              streams = "streams_landscape.tif",
                              d8_pointer = "d8_pointer_landscape.tif",
                              snap_dist,
                              save_map = TRUE){

  # Establish path to pour point shapefile
  if (!is.null(pour_point_file)){
    # Check that pour_point_file exists
    if(!file.exists(pour_point_file)){
      if(!file.exists(file.path(work_folder, pour_point_file))){
        stop("pour_point_file not found.")
      } else {
        pour_point_file <- file.path(work_folder, pour_point_file)
      }
    }
  } else {
    if (!is.null(pour_point_longlat)){
      # Create gis point in R
      pour_point <- sf::st_sf(sf::st_sfc(sf::st_point(x = pour_point_longlat, dim = "XYZ")))
      # Set crs as WGS84 (EPSG: 4326)
      sf::st_crs(pour_point) <- 4326
      # Change projection to UTM
      pour_point <- sf::st_transform(pour_point, crs = sf::st_crs(proj_epsg))
      # Save pour point so it can be imported to whitebox functions
      pour_point_file <- file.path(work_folder, "pour_point.shp")
      sf::st_write(pour_point, pour_point_file, delete_dsn = TRUE)
    } else {
      stop(noquote(paste("Please specify a pour_point")))
    }
  }

  # Check that streams exists.
  if(!file.exists(streams)){
    if(!file.exists(file.path(work_folder, streams))){
      stop("Streams not found.")
    } else {
      streams <- file.path(work_folder, streams)
    }
  }

  # Check that d8_pointer exists.
  if(!file.exists(d8_pointer)){
    if(!file.exists(file.path(work_folder, d8_pointer))){
      stop("d8_pointer not found.")
    } else {
      d8_pointer <- file.path(work_folder, d8_pointer)
    }
  }

  pour_point_snapped <- file.path(work_folder, "pour_point_snapped.shp")
  basin <- file.path(work_folder, "basin.tif")

  # -----------------

  print("***** Processing pour point *****")

  # Snap pour point to stream network
  whitebox::wbt_jenson_snap_pour_points(pour_pts = pour_point_file,
                                        streams = streams,
                                        output = pour_point_snapped,
                                        snap_dist = snap_dist)

  # Create basin file
  whitebox::wbt_watershed(d8_pntr = d8_pointer,
                          pour_pts = pour_point_snapped,
                          output = basin)

  # -----------------
  # Save a map showing pour point adjustment

  if (save_map){
    # Import streams_landscape.tif
    streams_input <- terra::rast(streams)
    # Import pour_point.shp
    pp_input <- sf::st_read(pour_point_file)
    # Import pour_point_snapped.shp
    pps_input <- sf::st_read(pour_point_snapped)

    jpeg(file.path(work_folder, "pour_point_comparison.jpg"))
    terra::plot(streams_input)
    terra::plot(pp_input, add = TRUE, col = "blue", pch = 19, cex = 1.5)
    terra::plot(pps_input, add = TRUE, col = "yellow", pch = 20, cex = 1)
    dev.off()
  }

  return(NULL)
}


