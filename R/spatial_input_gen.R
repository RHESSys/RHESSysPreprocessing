#' spatial_input_gen
#'
#' Generate the spatial inputs for RHESSys and RHESSysPreprocessing
#' @param name Name of your project
#' @param basin The area defining your basin/watershed of interest. Assumes it's a raster
#' @param DEM name of DEM map
#' @param GRASS_path Path to grass isntall location
#' @param threshold threshold for upstream patches to make a stream
#' @param r_output Should the resulting maps be output from the funciton into R
#' @param overwrite Should output files overwrite exisitng
#' @param basin optional input basin map
#' @param patch optional method to define patches
#' @param define_watershed Optional method to define watershed outline
#' @param easting easting
#' @param northing =northing
#' @param gisDbase gisDbase location
#' @param location GRASS GIS location
#' @param mapset GRASS GIS mapset
#' @author Will Burke
#' @export

spatial_input_gen = function(name,
                             DEM,
                             GRASS_path,
                             threshold,
                             r_output = FALSE,
                             overwrite = FALSE,
                             basin = NULL,
                             patch = FALSE,
                             define_watershed = FALSE,
                             easting = NULL,
                             northing = NULL,
                             gisDbase = NULL,
                             location = NULL,
                             mapset = NULL) {

  warning("Work in progress - May not work, output crs/proj is weird, optional args/fucntionality is broken, probably best to run line by line")

  # maybe this will help
  library(rgrass7)

  basin = NULL
  patch = FALSE
  define_watershed = FALSE
  easting = NULL
  northing = NULL
  gisDbase = NULL
  location = NULL
  mapset = NULL

  # check and convert file paths for args


  # check GRASS location and version - requires grass 7
  out_maps = NULL

  # set initial grass environment
  init = rgrass7::initGRASS(gisBase = GRASS_path,
                            home = tempdir(),
                            override = TRUE)
  # add basin map to new location
  if (!is.null(basin)) {
    rgrass7::execGRASS(cmd = "r.in.gdal", input = basin, output = "basin", location = "temp_loc")
    # set grass environment to new (correctly projected) location
    init = rgrass7::initGRASS(gisBase = GRASS_path,
                              home = tempdir(),
                              location = "temp_loc",
                              mapset = "PERMANENT",
                              override = TRUE)
    # check if same proj - should be quiet but idk
    check = rgrass7::execGRASS(cmd = "r.in.gdal",flags = c("j","quiet"), input = DEM)
    # if projections don't match
    if (check == 1) {
      # add DEM to its own location
      rgrass7::execGRASS(cmd = "r.in.gdal", input = DEM, output = "DEM", location = "dem_loc")
      # project DEM from that location to current loc (temp_loc)
      rgrass7::execGRASS(cmd = "r.proj", location = "dem_loc", mapset = "PERMANENT", input = "DEM")
    }
  } else {
    rgrass7::execGRASS(cmd = "r.in.gdal", input = DEM, output = "DEM", location = "dem_loc")
    init = rgrass7::initGRASS(gisBase = GRASS_path,
                              home = tempdir(),
                              location = "dem_loc",
                              mapset = "PERMANENT",
                              override = TRUE)
  }

  # --- checking env and stuff ---
  # rgrass7::gmeta()
  # rgrass7::execGRASS(cmd = "g.mapset", flags = "p")
  # rgrass7::execGRASS(cmd = "g.mapset", flags = "l")
  # rgrass7::execGRASS(cmd = "g.list", flags = "p", type = "raster")

  # check if basin exists and use it as region if it does, if not use raster
  if (!is.null(basin)) {
    rgrass7::execGRASS(cmd = "g.region", raster = "basin")
  } else {
    rgrass7::execGRASS(cmd = "g.region", raster = "DEM")
  }
  #rgrass7::execGRASS(cmd = "g.region", flags = "p") # print it

  # get slope and aspect maps
  rgrass7::execGRASS(cmd = "r.slope.aspect", elevation = "DEM", slope = "slope", aspect = "aspect")
  out_maps = c(out_maps, "slope", "aspect")

  # get horizon maps, in degrees
  rgrass7::execGRASS(cmd = "r.horizon", flags = "d", elevation = "DEM", direction = 0, output = "east")
  rgrass7::execGRASS(cmd = "r.horizon", flags = "d", elevation = "DEM", direction = 180, output = "west")
  out_maps = c(out_maps, "east_000", "west_180")

  rgrass7::execGRASS(cmd = "r.mapcalc", expression = 'east_horizon = sin(east_000)')
  rgrass7::execGRASS(cmd = "r.mapcalc", expression = 'west_horizon = sin(west_180)')
  out_maps = c(out_maps, "east_horizon", "west_horizon")

  #SET TO BE OPTIONAL
  #rgrass7::execGRASS(cmd = "r.mapcalc", expression = 'ehr.100 = east_horizon*100')
  #rgrass7::execGRASS(cmd = "r.mapcalc", expression = 'whr.100 = west_horizon*100')
  #out_maps = c(out_maps, "ehr.100", "whr.100")

  # watershed analysis
  subbasin_name = paste0("subbasin.t",as.character(threshold))
  stream_name = paste0("stream.t",as.character(threshold))
  hillslope_name = paste0("hillslope.t",as.character(threshold))

  rgrass7::execGRASS(cmd = "r.watershed", elevation = "DEM", threshold = threshold, accumulation = "accumulation",
                     drainage = "drainage", basin = subbasin_name, stream = stream_name, half_basin = hillslope_name)
  out_maps = c(out_maps, subbasin_name, stream_name, hillslope_name)

  # ----- patch map -----
  if (patch == "topo") {
  # Topographical
  hinfo = rgrass7::execGRASS(cmd = "r.info", map = hillslope_name, intern = TRUE)
  minmax_ch = unlist(strsplit(grep("Range of data: \\s+ min = \\d\\s+max = \\d",hinfo, value = TRUE),split = "\\s+"))
  hmax = as.numeric(minmax_ch[which(minmax_ch == "max") + 2])

  #rgrass7::execGRASS(cmd = "r.info", map = "DEM")
  rgrass7::execGRASS(cmd = "r.mapcalc", expression = paste0('p.topo = (DEM * ', hmax, ') + ', hillslope_name))
  rgrass7::execGRASS(cmd = "r.clump", input = "p.topo", output = "p.topo.cl")
  out_maps = c(out_maps, "p.topo.cl")

  } else if (patch == "grid") {
  # Grid
  rgrass7::execGRASS(cmd = "r.random.cells", output = "grid", distance = 0)
  rgrass7::execGRASS(cmd = "r.mapcalc", expression = paste0('p.grid = (grid * ', hmax, ') + ', hillslope_name))
  rgrass7::execGRASS(cmd = "r.clump", input = "p.grid", output = "p.grid.cl")
  out_maps = c(out_maps, "p.grid.cl")

  } else if (patch == "variable") {

    # THIS IS MOSTLY HARDCODED AND PROBABLY SHOULD BE ADDED TO A R-BASED FUNCTION THAT CAN DO IT WITH DYNAMIC RESOLUTION INPUTS
    # Variable resolution
    rgrass7::execGRASS(cmd = "g.region", raster = "DEM", res = 90)
    rgrass7::execGRASS(cmd = "r.mapcalc", expression = 'DEM90m = DEM')
    rgrass7::execGRASS(cmd = "g.region", raster = "DEM") #(reset the region back to 30m)
    rgrass7::execGRASS(cmd = "r.topidx", input = "DEM", output = "lna")
    rgrass7::execGRASS(cmd = "r.fillnulls", input = "lna", output = "lna.fill")
    rgrass7::execGRASS(cmd = "r.mapcalc", expression = 'lna.int = round(lna.fill)')
    rgrass7::execGRASS(cmd = "r.mapcalc", expression = 'riparian = lna.int > 7.5')
    rgrass7::execGRASS(cmd = "r.mapcalc", expression = paste0('p.rip30.up90 = (riparian == 1) * (DEM *', hmax,') + (riparian < 1)* (DEM90m * 633) + ', hillslope_name))
    rgrass7::execGRASS(cmd = "r.clump", input = "p.rip30.up90", output = "p.30_90res.cl")
    out_maps = c(out_maps, "p.30_90res.cl")

  } else if (patch == "aspatial_wetness") {
    # ALSO DIDNT TEST THIS ONE
    rgrass7::execGRASS(cmd =  "r.mapcalc", expression = paste0('p.lna = (lna.int * ', hmax,') + ', hillslope_name))
    out_maps = c(out_maps, "p.lna")

  }

  # Roads - zero map
  #rgrass7::execGRASS(cmd = "r.mapcalc", expression = 'zero = ("DEM" > 0)*0')

  if (define_watershed) {
    # ADDING AN INTERACTIVE GUI MAP HERE WOULD ALLOW FOR PRETTY SIMPLE BASIN DELINEATION
    rgrass7::execGRASS(cmd = "r.water.outlet", input = "drainage", output = "basin",coordinates = c(easting, northing))
    rgrass7::execGRASS(cmd = "d.rast", "basin")
    rgrass7::execGRASS(cmd = "g.copy", raster = "basin", 'MASK')
  }

  # Write rasters
  out_names = paste0(name,"_",out_maps,".tif")
  # check dir, maybe make new one
  #outdir = file.path(getwd(),dirname(name))
  outdir = dirname(name)
  if (!dir.exists(outdir)) {
    dir.create(dirname(name))
  }

  # make a new dir, this should be an option or something
  #dir.create(path = file.path(dirname(name), "out_maps") )

  outflags = c("c", "m", c("overwrite")[overwrite])

  for (i in 1:length(out_maps)) {
    cat("\n-- Writing '",out_maps[i] ,"', map ", i, " of ",length(out_maps)," --\n",sep = "")
    rgrass7::execGRASS(cmd = "r.out.gdal", flags = outflags, input = out_maps[i], output = out_names[i], format = "GTiff", type = "Float64")
  }

  if (r_output) {
    rgrass7::use_sp()
    rout = rgrass7::readRAST(vname = out_maps )
    return(rout)
  } else {
    return(NULL)
  }


}

