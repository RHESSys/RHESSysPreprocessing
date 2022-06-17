
<!-- README.md was generated from README.Rmd. Please edit that file -->

# RHESSysPreprocessing

R package providing preprocessing for the Regional HydroEcologic
Simulation System (RHESSys).

<!-- badges: start -->

[![R build
status](https://github.com/RHESSys/RHESSysPreprocessing/workflows/R-CMD-check/badge.svg)](https://github.com/RHESSys/RHESSysPreprocessing/actions)
[![Codecov test
coverage](https://codecov.io/gh/RHESSys/RHESSysPreprocessing/branch/master/graph/badge.svg)](https://codecov.io/gh/RHESSys/RHESSysPreprocessing?branch=master)
<!-- badges: end -->

## Overview

RHESSysPreprocessing is a set of functions to produce the required
inputs for RHESSys - notably the worldfile and flowtable. The package
also contains a variety of tools and utilities to facilitate other
RHESSys-related tasks.

## Installation

You can install RHESSysPreprocessing directly from R

``` r
# install.packages("devtools")
devtools::install_github("RHESSys/RHESSysPreprocessing")
```

<!-- Alternatively, the package can be installed manually: -->
<!-- 1. Download or clone the RHESSysPreprocessing repository. -->
<!-- 1. Open RHESSysPreprocessing.Rproj -->
<!-- 1. Under the "Build" menu, select "Install and restart". -->
<!-- 1. Close the package (under "File" - "Close Project"). -->
<!-- 1. Load the package via: `library(RHESSysPreprocessing)` -->

## Contents

RHESSysPreprocessing contains a few main components:

-   `RHESSysPreprocess()`: An all-in-one function that runs both
    `world_gen()` and `create_flownet()`
-   `world_gen()`: Creates a worldfile for use in RHESSys (replaces g2w)
-   `create_flownet()`: Creates a RHESSys flowtable (replaces cf)

Other functions of note that can be used on their own:

-   `template_read()`: Reads a template file and produces an R list
-   `update_template()`: Read, modify, and/or output a new template file
-   `read_world()`: Reads a worldfile and creates a dataframe with
    catagorized variables
-   `world_redefine()`: Small wrapper for `world_gen()` for creating a
    redefine worldfile
-   `read_in_flow()`: Reads a flow table and creates an R list
-   `convert_flowtable()`: Converts a non-parallelized (RHESSys \< 7.0)
    flowtable to a parallelized flowtable
-   `build_meta()`: Builds a metadata file. <under construction>
-   `spatial_input_gen()`: Expirimental all-in-one script to generate
    RHESSys spatial inputs/maps, uses GRASS 7

## Compatibility

Developed originally using R Version 3.4.0. May be incompatible with
older R versions Compatible with raster data - tested with GeoTIFF and
ASCII, but all standard GDAL formats should be supported. See
<https://www.gdal.org/formats_list.html> Previously was compatible with
GRASS GIS - versions 6.4.x and 7.x. Those GIS input methods are no
longer being actively supported and may be removed in future versions.

## Instructions

Prerequisites:

-   Input spatial data must be generated first. This includes level maps
    (world, basin, etc.) and maps of aspect, slope, horizon, etc. See:
    <https://github.com/RHESSys/RHESSys/wiki/Spatial-Input-Requirements>
    for more information on required and optional map inputs.  
-   Template - template must point to the spatial data you want to use.
    See: <https://github.com/RHESSys/RHESSys/wiki/Template>.

1.  All-in-one
    1)  Open the set-up/example vignette “Run_RHESSysPreprocess.Rmd”
    2)  Follow the instructions included in the script, and make edits
        to customize for your uses.
    3)  Script will run the RHESSysPreprocess.R function, and produce a
        worldfile and flowtable.
2.  Run separately
    1)  Run world_gen.R and create_flownet.R
    2)  For information on respective inputs, consult their help pages:
        `help(world_gen)` & `help(create_flownet)`

## Parallelization

To run the hillslope parallelized version of RHESSys (develop branch -
Dec, 2018), you must have a hillslope parallelized flowtable. This can
be done either by re-generating the flowtable from original inputs
(preferred), or converting an existing flowtable. Currently (1/14/19)
the develop branch should automatically produce a hillslope parallelized
flowtable.

Generate from original inputs:

1.  Setup RHESSysPreprocess.R or create_flownet.R as normal (see above
    instructions and included example R scripts)
2.  Set argument `parallel = TRUE`
3.  Set `make_stream` argument if desired (default is 4, can be set to
    any integer, or TRUE)
    -   The “make_stream” argument defines the distance from an existing
        stream where a patch/cell can be coerced to be a stream. Since
        all hillslopes must have stream outlets, if a hillslope outlet
        exists outside of the distance threshold set by “make_stream”,
        an error will occur and indicate the problem hillslope and
        outlet patch of that hillslope. This typically occurs as an
        artifact of how watershed analysis is done, and hillslopes are
        created, which sometimes results in fragmented or very
        small/skinny hillslopes, far away from streams.
    -   If you don’t care about fragmented hillslopes (and just want
        your flowtable to work), set “make_stream” to TRUE, and all
        hillslope outlets will become stream patches, and will route to
        the stream correctly.

Convert from existing:

1.  Use the convert_flowtable.R function and set your existing flowtable
    as the input.
2.  See above notes on the “make_stream” argument, which behaves the
    same here.

### Additional Information

The RHESSysPreprocessing package features R help documentation with more
information on each individual function, accessible via `?` or `help()`,
see in particular: `help(RHESSysPreprocess)`, `help(world_gen)`, and
`help(create_flownet)`.

A wide variety of information on RHESSys itself is available on the
RHESSys Github wiki page <https://github.com/RHESSys/RHESSys/wiki>

If you encounter bugs in RHESSysPreprocessing, please add an issue for
it!
