
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

data_dir <- system.file("test_datasets", package = "clueRIO", mustWork = TRUE)

regions <- st_read(dsn = paste0(data_dir, "/example_region.gpkg")) |> 
  mutate(region = "somewhere")

# landSystems <- tibble(ID = c(), system = c())

minimal <- clue_scene(
  name = "2lus_1demand_minimal", 
  period = c(2000, 2019),
  root = tempdir(), 
  description = "this is a minimal showcase where two landuse ...")

# first, specify the land system basis ...
minimal <- minimal |> 
  clue_landsystems(file = paste0(data_dir, "/landuse.tif"), 
                   regions = regions, 
                   cats = landSystems)

# ... then the drivers used to determine patterns
minimal <- minimal |> 
  clue_driver(file = paste0(data_dir, "/cropland_suit.tif"), 
              name = "suitCrop") |> 
  clue_driver(file = paste0(data_dir, "/forest_suit.tif"), 
              name = "suitFor")

# set land types up
minimal <- minimal |> 
  clue_landtype(name = "forest", resistance = 0.5,
                suitability = tibble(driver = "suitFor", 
                                     coef = 1)) |> 
  clue_landtype(name = "cropland", resistance = 0.5,
                suitability = tibble(driver = "suitCrop", 
                                     coef = 1), 
                production = tibble(commodity = "crops", 
                                    amount = 1))

# set commodities up
minimal <- minimal |> 
  clue_commodity(name = "crops", 
                 demand = tibble(year = c(2000:2019), 
                                 region = "somewhere", 
                                 amount = seq(from = 4760, to = 5000, length.out = 20)))

minimal <- minimal |> 
  clue_conversion(from = "forest", to = "cropland", label = "intensification")

# possibly adapt options
# clue_options(...)

# initiate the model
clue_initiate(scene = minimal)

# possibly check whether everything is set up properly
# clue_validate(scene = minimal)

# and finally run the model
clue_run(scene = minimal)
```
