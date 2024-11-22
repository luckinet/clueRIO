#' Set production of commodities by land-use types
#'
#' This function registers how much of a good or service (commodity) is produced
#' by land-use type(s).
#' @param of [`character(1)`][character]\cr a commoditiy for which a production
#'   amount should be set.
#' @param by [`character(.)`][character]\cr land-use type(s) by which \code{of}
#'   is produced.
#' @param amount [`numeric(.)`][numeric]\cr the amount(s) of produced (must have
#'   same length as \code{by}).
#' @param unit [`character(1)`][character]\cr the unit of \code{amount}.
#' @param priority [`integerish(1)`][integer]\cr how likely the commodity is
#'   produced by the land-use type(s) in response to other land-use types.
#' @param exclude [`character(.)`][character]\cr land-use types from which the
#'   commodity is excluded to be produced. Overrides the other arguments if set.
#' @details A commodity can be any good or service and it can be produced by
#'   several land-use types. However, often distinct land-use types produce
#'   different amounts of the commodities and they produce them with a different
#'   likeliness. This likeliness is specified as land-use type specific
#'   \code{priority} value.
#' @family functions to specify commodities
#' @importFrom checkmate assertCharacter assertNumeric assert assertIntegerish
#'   assertChoice
#' @export

setProduction <- function(of = NULL, by = NULL, amount = NULL, priority = NULL,
                          exclude = NULL){

  .Deprecated("clue_landtype")

  opts <- getOption("lclu")

  assertCharacter(x = of, any.missing = FALSE, len = 1)
  assertCharacter(x = by, any.missing = FALSE, null.ok = TRUE)
  assertNumeric(x = amount, any.missing = FALSE, lower = 0, null.ok = TRUE)
  assert(length(by) == length(amount))
  assertIntegerish(x = priority, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = exclude, any.missing = FALSE, null.ok = TRUE)

  # test that 'name' is a valid demand type
  targetDemands <- opts$attributes$demand$type
  targetSystems <- opts$attributes$landuse$system
  assertChoice(x = of, choices = targetDemands)

  # set production in lusmatrix.txt and priority in lusconv.txt
  lusMat <- opts$tables$lusmatrix
  lusConv <- opts$tables$lusconv

  if (!is.null(exclude)) {
    theAmount <- 0
    thePrio <- -1
    by <- exclude
  } else {
    theAmount <- amount
    if(is.null(priority)){
      lusConv[of][lusConv[[of]] != 0,] <- lusConv[of][lusConv[[of]] != 0,] + 1
      thePrio <- 1
    } else {
      thePrio <- priority
    }
  }

  targetSystems <- opts$attributes$landuse$system
  for(i in seq_along(by)){
    assertChoice(x = by[i], choices = targetSystems)
  }

  lusMat[of][which(lusMat$system %in% by),] <- theAmount
  lusConv[of][which(lusConv$system %in% by),] <- thePrio
  opts$tables$lusmatrix <- lusMat
  opts$tables$lusconv <- lusConv

  options(lclu = opts)

  invisible(of)
}
