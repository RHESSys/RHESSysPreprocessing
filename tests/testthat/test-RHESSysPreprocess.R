#context("RHESSys-Preprocess testing")
library(RHESSysPreprocessing)

RHESSysPreprocess(template = system.file("extdata", "test2.template", package = "RHESSysPreprocessing"),
                  name = file.path(system.file("extdata", package = "RHESSysPreprocessing"), "test_out"),
                  type = "raster",
                  typepars = system.file("extdata", package = "RHESSysPreprocessing"),
                  streams = "streams",
                  overwrite = T)

# generate reference
# ref_world = read_world(worldfile = file.path(system.file("extdata", package = "RHESSysPreprocessing"), "test_out.world"))
# ref_flow = read_in_flow(input_file = file.path(system.file("extdata", package = "RHESSysPreprocessing"), "test_out.flow"))
# save(ref_world, file = "inst/extdata/ref_worldfile")
# save(ref_flow, file = "inst/extdata/ref_flownet")

test_that("Worldfile is generated", {
  test_world = read_world(worldfile = file.path(system.file("extdata", package = "RHESSysPreprocessing"), "test_out.world"))
  load(system.file("extdata", "ref_worldfile", package = "RHESSysPreprocessing"))
  expect_equal(test_world, ref_world)
})

test_that("Flowtable is generated", {
  test_flow = read_in_flow(input_file = file.path(system.file("extdata", package = "RHESSysPreprocessing"), "test_out.flow"))
  load(system.file("extdata", "ref_flownet", package = "RHESSysPreprocessing"))
  expect_equal(test_flow, ref_flow)
})

test_that("RHESSysPreproccess all-in-one function works", {

})

