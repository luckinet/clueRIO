#' Set land types and their attributes
#'
#' @param scene description
#' @param name [`character(1)`][character]\cr name of the land type.
#' @param resistance [`numeric(1)`][numeric]\cr the relative conversion
#'   resistance to land change.
#' @param limit [`integerish(1)`][integer]\cr the number of time steps a
#'   land-use type is allowed to remain in a location at most.
#' @param production [`data.frame(1)`][data.frame]\cr table of what, with which
#'   priority and how much this land type produces. Must have names
#'   \code{commodity}, \code{priority} and \code{amount}.
#' @param suitability [`data.frame(1)`][data.frame]\cr table of driver and
#'   coefficient of suitability. Must have names \code{driver} and \code{coef}.
#'   Driver names must correspond to the names defined in
#'   \code{\link{clue_driver}}.
#' @details Additional details...
#' @return
#' @importFrom checkmate assertClass assertCharacter assertDataFrame assertNames
#' @export

clue_landtype <- function(scene, name, resistance = NULL, limit = NULL,
                          suitability = NULL, production = NULL){

  assertClass(x = scene, classes = "scene")
  assertCharacter(x = name, len = 1, any.missing = FALSE)
  assertDataFrame(x = suitability, ncols = 2, all.missing = FALSE, null.ok = TRUE)
  if(!is.null(suitability)){
    assertNames(x = names(suitability), permutation.of = c("driver", "coef"))
  }
  assertDataFrame(x = production, ncols = 3, all.missing = FALSE, null.ok = TRUE)
  if(!is.null(production)){
    assertNames(x = names(production), permutation.of = c("commodity", "priority", "amount"))
  }

  # test that 'name' is a valid land system
  assertChoice(x = name, choices = scene@landsystems$system)

  temp <- list(resistance = resistance,
               limit = limit,
               suitability = suitability,
               production = production)

  scene@landtypes[[name]] <- temp

  return(scene)
}
