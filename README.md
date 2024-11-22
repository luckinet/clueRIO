
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clueRIO <a href='https://github.com/luckinet/cluerio/'><img src='man/figures/logo.svg' align="right" height="200" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/cluerio)](https://CRAN.R-project.org/package=cluerio)

[![R-CMD-check](https://github.com/luckinet/cluerio/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/luckinet/cluerio/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/luckinet/cluerio/branch/main/graph/badge.svg)](https://app.codecov.io/gh/luckinet/cluerio?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

The
[CLUMondo](https://www.environmentalgeography.nl/site/data-models/models/clumondo-model/)
model is a dynamic, spatially explicit, land use and land cover change
model. clueRIO manages input/output for setting up the file-structure
required for successfully configuring and running a CLUE model. This
acts as an interface that enables users to build and document scenarios
based on such models quickly and in a script-based fashion.

The most up-to-date version is typically a command line version. This
R-package has the aim to simplify specifying all files needed to
successfully run that version. Please make sure to reference not only
this R-package, but also the references on the website and in particular
*Van Asselen, S., Verburg, P.H., 2013. Land cover change or land-use
intensification: simulating land system change with a global-scale land
change model. Global Change Biology 19, 3648–3667*.

## Installation

Install the official version from CRAN:

``` r
# install.packages("clueRIO")
```

Install the latest development version from github:

``` r
devtools::install_github("luckinet/clueRIO")
```

## Examples

``` r
library(clueRIO)
library(dplyr)
library(sf)
library(terra)

data <- system.file("test_data", package = "clueRIO", mustWork = TRUE)

# open a new scene for the model
region <- st_read(dsn = paste0(data, "/test_region.gpkg"))

minimal <- clue_scene(
  root = paste0(tempdir(), "/ls2d1"), 
  name = "ls2d1", 
  period = c(2000, 2024),
  regions = region,
  description = "this is a minimal showcase where forest is converted to cropland to produce crops.")

# first, specify the land system basis ...
landsystems <- rast(x = paste0(data, "/ls2.tif"))
systems <- tibble(ID = c(0, 1), 
                  type = c("forest", "cropland"))

minimal <- minimal |> 
  clue_landsystems(file = landsystems, cats = systems)

# possibly set a mask
# clue_mask(file = ...)

# ... then the drivers used to determine patterns
minimal <- minimal |> 
  clue_driver(file = paste0(data, "/suitability1.tif"), 
              name = "cropland_suit") |> 
  clue_driver(file = paste0(data, "/suitability2_0.tif"), 
              name = "forest_suit")

# set land types up
minimal <- minimal |> 
  clue_landtype(name = "forest", resistance = 0.5,
                suitability = tibble(driver = "forest_suit", 
                                     coef = 1),
                conversion = tibble(to = "cropland",
                                    label = "intensification")) |> 
  clue_landtype(name = "cropland", resistance = 0.5,
                suitability = tibble(driver = "cropland_suit", 
                                     coef = 1), 
                production = tibble(commodity = "crops", 
                                    amount = 1,
                                    priority = 1))

# set commodities up
minimal <- minimal |> 
  clue_commodity(name = "crops", 
                 demand = tibble(year = c(2000:2019), 
                                 region = "ravenholm", 
                                 amount = seq(from = 4760, to = 5000, length.out = 20)))

# possibly adapt options
# clue_options(...)

# initiate the model
clue_initiate(scene = minimal)

# possibly check whether everything is set up properly
# clue_validate(scene = minimal)

# and finally run the model
clue_run(scene = minimal)
```
