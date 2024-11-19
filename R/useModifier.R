#' Modify parts of a model
#'
#' @param module [`character(1)`][character]\cr modifications of CLUMondo
#'   typically come with a set of files that have to be adapted and these are
#'   summarised into modules. By selecting a module you thus make sure that all
#'   adaptions are done consistently.
#' @param ... module specific objects for that modification. See documentation
#'   of the respective module.
#' @details Availble modules \itemize{ \item \code{\link{mod_crop_rotation}}:
#'   When using this module, CLUMondo will be run separately for each year by
#'   default and re-calculate \code{'locspec'} after each step.}
#' @importFrom checkmate assertChoice assertNames
#' @importFrom rlang exprs
#' @export

useModifier <- function(module = "crop_rotation", ...){

  opts <- getOption("lclu")
  assertChoice(x = module, choices = c("crop_rotation"))
  obj <- exprs(..., .named = TRUE)

  if(module == "crop_rotation"){
    opts$meta$stepwise <- TRUE
    opts$modifier <- c(opts$modifier, list("crop_rotation" = list()))
  }
#
#   if(module == "dynamic_lus"){
#     assertNames(x = names(obj), permutation.of = c("table"))
#     opts$modifier <- c(opts$modifier, list("dynamic_lus" = obj))
#   }

  options(lclu = opts)

}