context("RHESSys-Preprocess testing")
library(RHESSysPreprocessing)

test_that("Template is parsed correctly", {

  file = system.file("extdata", "test.template", package = "RHESSysPreprocessing")
  #file = "../rhessys/RHESSysPreprocessing/inst/extdata/test.template"
  template = template_read(template = file)

  expect_length(template, 6)
  expect_equal(template[[3]], c(1L, 2L, 9L, 17L, 30L, 64L) )
  expect_equal(template[[5]],
               structure(c("world", "basin", "hillslope", "zone", "patch", "strata",
                           "z", "z", "z", "z", "basin_bigcreek", "basin_bigcreek", "basin_bigcreek",
                           "basin_bigcreek", "basin_bigcreek", "basin_bigcreek", "DEM_bigcreek",
                           "DEM_bigcreek", "DEM_bigcreek", "DEM_bigcreek"),
                         .Dim = c(10L, 2L), .Dimnames = list(NULL, c("MapName", "Map"))))


})



# expect_file = function(file) {
#   file.exists(file)
# }
#
# test_that("Creates worldfile", {
#
#
# })
