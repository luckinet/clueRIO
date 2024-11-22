#' Set land types and their attributes
#'
#' @param scene description
#' @param name [`character(1)`][character]\cr name of the land type.
#' @param resistance [`numeric(1)`][numeric]\cr the relative conversion
#'   resistance to land change.
#' @param limit [`integerish(1)`][integer]\cr the number of time steps a
#'   land-use type is allowed to remain in a location at most.
#' @param production [`data.frame(3)`][data.frame]\cr table of what, with which
#'   priority and how much this land type produces. Must have names
#'   \code{'commodity'}, \code{'priority'} and \code{'amount'}.
#' @param suitability [`data.frame(2)`][data.frame]\cr table of driver and
#'   coefficient of suitability. Must have names \code{'driver'} and
#'   \code{'coef'}. Driver names must correspond to the names defined in
#'   \code{\link{clue_driver}}.
#' @param conversion [`data.frame(.)`][data.frame]\cr table of land types into
#'   which the land type can be converted. Must have names \code{'to'} and
#'   \code{'label'} and optionally one of \code{'thresh'}, \code{'auto'} or
#'   \code{'mask'}.
#' @param neighborhood [`data.frame(1)`][data.frame]\cr work in progress
#' @param preference [`data.frame(2)`][data.frame]\cr table of where and how
#'   location specific preferences are used. Must have names \code{'layer'} and
#'   \code{'weight'}.
#' @details
#' @return
#' @examples
#' @importFrom checkmate assertClass assertCharacter assertDataFrame assertNames
#'   assertTRUE
#' @importFrom tibble add_column
#' @export

clue_landtype <- function(scene, name, resistance = NULL, limit = NULL,
                          suitability = NULL, production = NULL,
                          conversion = NULL, neighborhood = NULL,
                          preference = NULL){

  assertClass(x = scene, classes = "scene")
  assertCharacter(x = name, len = 1, any.missing = FALSE)
  assertDataFrame(x = suitability, ncols = 2, all.missing = FALSE, null.ok = TRUE)
  if(!is.null(suitability)){
    assertNames(x = names(suitability), permutation.of = c("driver", "coef"))
  }
  assertDataFrame(x = production, min.cols = 2, max.cols = 3, all.missing = FALSE, null.ok = TRUE)
  if(!is.null(production)){
    assertNames(x = names(production), must.include = c("commodity", "priority", "amount"))
  }
  assertDataFrame(x = conversion, min.cols = 2, max.cols = 3, all.missing = FALSE, null.ok = TRUE)
  if(!is.null(conversion)){
    assertNames(x = names(conversion), must.include = c("to", "label"), subset.of = c("to", "label", "thresh", "auto", "mask"))
  }
  assertDataFrame(x = neighborhood, all.missing = FALSE, null.ok = TRUE)
  if(!is.null(neighborhood)){
    assertNames(x = names(neighborhood), subset.of = c("const", "weight", "kernel"))
  }
  assertDataFrame(x = preference, nrows = 2, all.missing = FALSE, null.ok = TRUE)
  if(!is.null(preference)){
    assertNames(x = names(preference), must.include = c("layer", "weight"))
  }

  # test that 'name' is a valid land system
  assertChoice(x = name, choices = scene@landsystems$type)

  temp <- list(resistance = resistance,
               limit = limit,
               production = production,
               suitability = suitability,
               conversion = conversion,
               neighborhood = neighborhood,
               preference = preference)

  scene@landtypes[[name]] <- temp

  return(scene)
}
