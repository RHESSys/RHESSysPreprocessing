context("GIS_read testing")
library(RHESSysPreprocessing)

# TEMPORARY
# setwd("/Users/burke/Google Drive/UCSB/Research/rhessys/RHESSysPreprocessing/")

# Basic inputs
map_info = matrix(data = c("world", "basin", "hillslope", "zone", "patch", "strata", rep("map",6)),
                  ncol = 2, dimnames = list(NULL, c("MapName", "Map")))
type = "raster"
typepars = system.file("extdata", package = "RHESSysPreprocessing")
# typepars = "inst/extdata/"

test_that("GeoTIFF spatial data can be read", {
  map_info[,2] = c("basin.tif", "basin.tif", "patches.tif", "patches.tif", "patches.tif", "patches.tif")
  gis_test = GIS_read(unique(map_info[,2] ), type, typepars, map_info)

  # there's two maps
  expect_length(gis_test@data, 2)
  # data is the same
  expect_equal(gis_test@data$basin.tif, rep(1,9))
  expect_equal(gis_test@data$patches.tif, c(13319, 13320, 13321, 13485, 13486, 13487, 13646, 13647, 13648))
  # projection is retained
  expect_equal(gis_test@proj4string,
               new("CRS", projargs = "+proj=utm +zone=11 +datum=NAD27 +units=m +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat"))
  # cellsize and bounding box are the same
  expect_equal(gis_test@grid@cellsize, c(30.0019175823351, 30.0019176000108))
  expect_equal(as.vector(gis_test@bbox), c(303919.422441758, 4103542.2433688, 304009.428194505, 4103632.2491216))
})

test_that("ASCII spatial data can be read", {


})

test_that("ASCII background 0 vs NA resolve correctly", {


})

test_that("seq_patch_IDs arg works", {
  #gis_test = GIS_read(maps_in = maps_in, type = type, typepars = typepars, map_info = map_info, seq_patch_IDs = TRUE, output_patch_map = FALSE)
})

test_that("output_patch_map arg works", {
  #gis_test = GIS_read(maps_in = maps_in, type = type, typepars = typepars, map_info = map_info, seq_patch_IDs = TRUE, output_patch_map = TRUE)
})

test_that("unprojected maps work", {

})


