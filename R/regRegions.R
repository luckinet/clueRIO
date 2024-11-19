#' Set distinct regions to model
#'
#' This function registers the spatial outline of one or more regions.
#' @param features [`sf(.)`][sf]\cr collection of simple features that outline
#'   the distinct regions for each of which an iteration of the model shall be
#'   run.
#' @param region [`character(1)`][character]\cr the column/variable in
#'   \code{features} that distinguishes unique regions of that object. The
#'   values in this column need to correspond to \code{region} in
#'   \code{\link{regDemand}}.
#' @details The CLUMondo model is currently meant to be run for a distinct
#'   region only. A project with several regions (e.g., territorial units) is
#'   basically split up into several runs of the model, each for a specific
#'   region.
#' @family functions to register items
#' @importFrom checkmate assertClass assertList
#' @export

regRegions <- function(features = NULL, region = NULL){

  opts <- getOption("lclu")

  assertClass(x = features, classes = "sf")
  assertCharacter(x = region, any.missing = FALSE, len = 1)

  opts$data$regions <- features

  options(lclu = opts)

}