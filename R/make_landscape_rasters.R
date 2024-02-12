#' Make landscape rasters for watershed processing
#'
#' Using a DEM input, this function generates aspect, slope, horizon (east and
#' west), d8 pointer, d8 flow accumulation, stream network, subbasin, and patch
#' rasters.
#'
#' @param dem Filename&extension (if located in work_folder), or
#'   path&filename&extension, of the DEM to be inputted. DEM must be a file on
#'   hard drive, not an object within R. The function assumes that the DEM is
#'   already at the desired resolution, extent and projection. Projection should
#'   be in UTM, as some of the whitebox computations assume that pixel
#'   dimensions are in meters.
#' @param work_folder Path to folder containing inputs/outputs
#' @param stream_threshold Threshold of flow accumulation for designating a
#'   patch as stream.
#' @param patch_method Options for generating patch raster. Options include
#'   'simple' (default).
#'
#' @author Ryan Bart
#'
#' @section Note: WhiteboxTools Open Core (https://www.whiteboxgeo.com/) must be
#'   installed on your computer to run this function. The R package whitebox is
#'   an R wrapper that interfaces with Whitebox.
#'
#'   The raster outputs from this function include filenames indicating the type
#'   of data plus an appended "_landscape". The "_landscape" signifies that the
#'   raster values encompasses the entire bounding box, as opposed to simply the
#'   basin, which will be generated in subsequent functions.
#'
#'   Links to embedded whitebox functions
#'   https://www.whiteboxgeo.com/manual/wbt_book/available_tools/geomorphometric_analysis.html#Aspect
#'   https://www.whiteboxgeo.com/manual/wbt_book/available_tools/geomorphometric_analysis.html#Slope
#'   https://www.whiteboxgeo.com/manual/wbt_book/available_tools/geomorphometric_analysis.html#HorizonAngle
#'   https://www.whiteboxgeo.com/manual/wbt_book/available_tools/hydrological_analysis.html#BreachDepressions
#'   https://www.whiteboxgeo.com/manual/wbt_book/available_tools/hydrological_analysis.html#D8Pointer
#'   https://www.whiteboxgeo.com/manual/wbt_book/available_tools/hydrological_analysis.html#D8FlowAccumulation
#'   https://www.whiteboxgeo.com/manual/wbt_book/available_tools/stream_network_analysis.html#ExtractStreams
#'   https://www.whiteboxgeo.com/manual/wbt_book/available_tools/hydrological_analysis.html#Subbasins
#'
#' @export

make_landscape_rasters <- function(dem,
                                   work_folder,
                                   stream_threshold,
                                   patch_method = "simple"){

  # Check that whitebox is loaded
  if (whitebox::check_whitebox_binary() == FALSE){stop("Whitebox not detected.")}

  # Check that work folder exists. If it doesn't, then make it, if possible.
  if(!dir.exists(work_folder)){dir.create(work_folder)}

  # Check that DEM exists.
  if(!file.exists(dem)){
    if(!file.exists(file.path(work_folder, dem))){
      stop("DEM not found.")
    } else {
      dem <- file.path(work_folder, dem)
    }
  }

  # -----------------
  # Process rasters with Whitebox

  print("***** Processing DEM *****")

  # Aspect
  whitebox::wbt_aspect(dem = dem,
                       output = file.path(work_folder, "aspect_landscape.tif"))

  # Slope
  whitebox::wbt_slope(dem = dem,
                      output = file.path(work_folder, "slope_landscape.tif"))

  # East horizon
  whitebox::wbt_horizon_angle(dem = dem,
                              output = file.path(work_folder, "ehr_landscape.tif"),
                              azimuth = 90,
                              max_dist = 100000)    # Tests for horizon up to 100 km from patch

  # West horizon
  whitebox::wbt_horizon_angle(dem = dem,
                              output = file.path(work_folder, "whr_landscape.tif"),
                              azimuth = 270,
                              max_dist = 100000)    # Tests for horizon up to 100 km from patch

  # Breach Depressions
  whitebox::wbt_breach_depressions(dem = dem,
                                   output = file.path(work_folder, "dem_breach_landscape.tif"))

  # D8 (8-direction) pointer
  whitebox::wbt_d8_pointer(dem = file.path(work_folder, "dem_breach_landscape.tif"),
                           output = file.path(work_folder, "d8_pointer_landscape.tif"))

  # D8 (8-direction) flow accumulation
  whitebox::wbt_d8_flow_accumulation(input = file.path(work_folder, "dem_breach_landscape.tif"),
                                     output = file.path(work_folder, "d8_flow_accumulation_landscape.tif"))

  # Extract streams
  whitebox::wbt_extract_streams(flow_accum = file.path(work_folder, "d8_flow_accumulation_landscape.tif"),
                                output = file.path(work_folder, "streams_landscape.tif"),
                                threshold = stream_threshold)

  # Subbasins
  whitebox::wbt_subbasins(d8_pntr = file.path(work_folder, "d8_pointer_landscape.tif"),
                          streams = file.path(work_folder, "streams_landscape.tif"),
                          output = file.path(work_folder, "subbasin_landscape.tif"))

  # -----------------
  # Create patch maps

  # Simple patch raster (Unique patch ID for each dem pixel. No grouping.)
  if (patch_method == "simple"){
    dem <- terra::rast(dem)
    terra::values(dem) <- seq(1,terra::ncell(dem))
    terra::writeRaster(dem, filename = file.path(work_folder, "patch_landscape.tif"), overwrite=TRUE)
  }

  return(NULL)
}


