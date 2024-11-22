#' Set commodities
#'
#' This function registers commodities and associates them to land-use types.
#' @param name [`character(1)`][character]\cr the name of the commodity.
#' @param match [`character(1)`][character]\cr the type of match for this commodity,
#'   either \code{"exact"} (the default), \code{"minimum"} (the demand indicated
#'   is the minimum demand that needs to be achieved, any overshoot is accepted)
#'   or \code{"maximum"} (the demand indicated is the maximum that may be
#'   reached, any undershoot is accepted).
#' @family functions to set new items
#' @importFrom checkmate assertCharacter
#' @export

regCommodity <- function(name = NULL, match = "exact", table = NULL){

  .Deprecated("clue_commodity")

  opts <- getOption("lclu")

  assertCharacter(x = name, any.missing = FALSE, len = 1)
  assertDataFrame(x = table, any.missing = FALSE, null.ok = TRUE)
  assertChoice(x = match, choices = c("exact", "minimum", "maximum"))
  if (!is.null(table)) {
    assertNames(x = colnames(table), must.include = c("name", "demand"))
  }

  # test that 'name' is a valid demand type
  targetDemands <- opts$attributes$demand$type
  assertChoice(x = name, choices = targetDemands)

  # update demand type
  newDemand <- opts$attributes$demand
  if(match == "exact"){
    dt <- 1
  } else if (match == "minimum"){
    dt <- -1
  } else {
    dt <- 101
  }
  newDemand["match"][which(newDemand$type == name),] <- as.character(dt)

  opts$attributes$demand <- newDemand

  options(lclu = opts)

  invisible(name)
}
