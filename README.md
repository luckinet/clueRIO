
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

# Installation

1.  Install the official version from CRAN:

``` r
# install.packages("clueRIO")
```

or the latest development version from github:

``` r
devtools::install_github("luckinet/clueRIO")
```

2.  Read the mentioned paper and the CLUMondo documentation.
3.  Read the vignette on …

- [Getting
  Started](https://luckinet.github.io/clueRIO/articles/getting_started.html)
- [Use
  Cases](https://luckinet.github.io/clueRIO/articles/use_cases.html)
