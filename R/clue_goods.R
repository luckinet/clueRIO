#' Set goods or services and their attributes
#'
#' @param scene description
#' @param name [`character(1)`][character]\cr name of the commodity.
#' @param excluded [`character(.)`][character]\cr land types from which the
#'   commodity is excluded to be produced.
#' @param match [`character(1)`][character]\cr the type of match for this
#'   commodity, either \code{"exact"} (the default), \code{"minimum"} (the
#'   demand indicated is the minimum demand that needs to be achieved, any
#'   overshoot is accepted) or \code{"maximum"} (the demand indicated is the
#'   maximum that may be reached, any undershoot is accepted).
#' @param demand [`data.frame(1)`][data.frame]\cr table of when, where and how
#'   much of the commodity is produced. Must have names \code{year},
#'   \code{region} and \code{amount}.
#' @details
#' @return
#' @importFrom checkmate assertClass assertCharacter assertChoice
#'   assertDataFrame assertNames
#' @export

clue_goods <- function(scene, name, excluded = NULL, match = "exact",
                       demand = NULL){

  assertClass(x = scene, classes = "scene")
  assertCharacter(x = name, len = 1, any.missing = FALSE)
  assertCharacter(x = excluded, any.missing = FALSE, null.ok = TRUE)
  assertChoice(x = match, choices = c("exact", "minimum", "maximum"))
  assertDataFrame(x = demand, ncols = 3, all.missing = FALSE, null.ok = TRUE)

  if(is.null(excluded)){
    excluded <- tibble(type = NA_character_)
  } else{
    excluded <- tibble(type = excluded)
  }
  excluded <- excluded |>
    nest()

  # update demand type
  if(match == "exact"){
    dt <- 1
  } else if (match == "minimum"){
    dt <- -1
  } else {
    dt <- 101
  }

  if(!is.null(demand)){
    assertNames(x = names(demand), permutation.of = c("year", "region", "amount"))
  }
  demand <- demand |>
    nest()

  temp <- list(match = as.character(dt),
               excluded = excluded,
               demand = demand)

  scene@goods[[name]] <- temp

  return(scene)
}
