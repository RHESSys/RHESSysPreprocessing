#context("GIS_read testing")
library(RHESSysPreprocessing)

# ---------- TEMPORARY ----------
# library(testthat)
# setwd("~/Repos/RHESSysPreprocessing/")
# map_dir = "inst/extdata/"
# --------------------

# Basic inputs
map_info = matrix(data = c("world", "basin", "hillslope", "zone", "patch", "strata", rep("map",6)),
                  ncol = 2, dimnames = list(NULL, c("MapName", "Map")))

map_dir = system.file("extdata", package = "RHESSysPreprocessing")


test_that("GeoTIFF spatial data can be read", {
  map_info[,2] = c("basin.tif", "basin.tif", "basin.tif", "patches.tif", "patches.tif", "patches.tif")
  gis_test = GIS_read(maps_in = unique(map_info[,2] ), map_dir = map_dir, map_info = map_info)
  map_df = as.data.frame(gis_test)

  # there's two maps
  expect_length(map_df, 2)
  # data is the same
  expect_equal(map_df$basin.tif, rep(1,9))
  expect_equal(map_df$patches.tif, c(13319, 13320, 13321, 13485, 13486, 13487, 13646, 13647, 13648))
  # projection is retained
  expect_equal(as.character(terra::crs(gis_test, proj = T)),
               as.character("+proj=utm +zone=11 +datum=NAD27 +units=m +no_defs"))
  # cellsize and bounding box are the same
  expect_equal(terra::res(gis_test), c(30.0019175823351, 30.0019176000108))
  expect_equal(as.numeric(terra::ext(gis_test)[1:4]), c(303919.422441758, 304009.428194505,4103542.2433688, 4103632.2491216))
})

test_that("ASCII spatial data (no projection) works", {
  map_info[,2] = c("basin_grass.asc", "basin_grass.asc", "basin_grass.asc", "patches_grass.asc", "patches_grass.asc", "patches_grass.asc")
  gis_test = GIS_read(unique(map_info[,2] ), map_dir = map_dir, map_info = map_info)
  map_df = as.data.frame(gis_test)

  # there's two maps
  expect_length(map_df, 2)
  # data is the same
  expect_equal(map_df$basin_grass.asc, rep(1,9))
  expect_equal(map_df$patches_grass.asc, c(13319, 13320, 13321, 13485, 13486, 13487, 13646, 13647, 13648))
  # no projection on grass ascii - why it's kinda worse
  # cellsize and bounding box are the same
  expect_equal(terra::res(gis_test), c(30.0019175823351, 30.0019176000108))
  expect_equal(as.numeric(terra::ext(gis_test)[1:4]), c(303919.422441758, 304009.428194505,4103542.2433688, 4103632.2491216))
})

test_that("ASCII background NULL (*) are read correctly", {
  # this could be more elaborate -
  # I set the world as the single cell, check if the others are cropped/masked correctly
  map_info[,2] = c("one_cell_grass.asc", "basin_grass.asc", "basin_grass.asc", "patches_grass.asc", "patches_grass.asc", "patches_grass.asc")
  gis_test = GIS_read(unique(map_info[,2] ), map_dir = map_dir, map_info = map_info)
  map_df = as.data.frame(gis_test)
  expect_length(map_df$patches_grass.asc, 1)
})

test_that("seq_patch_IDs arg works", {
  map_info[,2] = c("basin.tif", "basin.tif", "basin.tif", "patches.tif", "patches.tif", "patches.tif")
  gis_test = GIS_read(unique(map_info[,2]), map_dir = map_dir, map_info = map_info, seq_patch_IDs = TRUE, output_patch_map = FALSE)
  map_df = as.data.frame(gis_test)
  expect_equal(map_df$patches.tif, seq_along(map_df$patches.tif))
})

test_that("output_patch_map arg works", {
  if (file.exists(system.file("extdata", "patches_seqID.tif", package = "RHESSysPreprocessing"))) {
    file.remove(system.file("extdata", "patches_seqID.tif", package = "RHESSysPreprocessing"))
  }
  map_info[,2] = c("basin.tif", "basin.tif", "basin.tif", "patches.tif", "patches.tif", "patches.tif")
  gis_test = suppressWarnings(GIS_read(unique(map_info[,2]), map_dir = map_dir, map_info = map_info, seq_patch_IDs = TRUE, output_patch_map = TRUE))
  expect_true(file.exists(system.file("extdata", "patches_seqID.tif", package = "RHESSysPreprocessing")))
})

