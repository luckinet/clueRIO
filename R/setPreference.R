#' Set a location specific preference
#'
#' @param landuse [`character(1)`][character]\cr the landuse type for which to
#'   specify location specific preference.
#' @param layer [`character(1)`][character]\cr the gridded object in which the
#'   location specific preference values are noted. If this is left empty, it
#'   will be a layer that includes only the value 0 for all pixels.
#' @param weight [`numeric(1)`][numeric]\cr the weight (beween -1 for
#'   down-weighing and 1 for up-weighing the preferences) assigned to this
#'   factor.
#' @param subset [`character(1)`][character]\cr an expression that successfully
#'   evaluates to the subset of a raster, for example to subset a layer on the
#'   fly between years.
#' @param table [`data.frame(1)`][data.frame]\cr table of information on
#'   location specific preference. Must contain the columns \code{landuse},
#'   \code{layer}, and \code{weight}.
#' @details \code{layer = "self"} is reserved to derive dynamic locspec maps. In
#'   that case the age map of each timestep is multiplied by \code{weight}.
#'   Depending on the sign of weight, either all age-value of this class
#'   (negative) weight) or of all other classes (positive weight) are selected
#'   for multiplication (all others are set to 0). De facto, this leads to
#'   decrease of preference where the class already occurs with a negative sign,
#'   and increase of preference where the class does not yet exist with a
#'   positive sign. This can be interessting, when a class becomes less likely,
#'   the longer it is present, e.g., in crop rotation schemes, where both,
#'   ressource depletion or pathogen pressure but also increasing times of
#'   commodities that repair the soil, make rotation more likely.
#' @family functions to specify landuse
#' @importFrom checkmate assertTibble assertNumeric assertCharacter assertNames
#'   assertChoice assertSubset assertIntegerish
#' @importFrom stringr str_trim
#' @export

setPreference <- function(landuse = NULL, layer = NULL, weight = NULL, table = NULL){

  .Deprecated("clue_landtype")

  opts <- getOption("lclu")

  assertTibble(x = table, min.cols = 3, nrows = length(opts$attributes$landuse$system), null.ok = TRUE)
  if(!is.null(table)){
    assertNames(x = names(table), must.include = c("landuse"))
    assertNames(x = table$landuse, permutation.of = opts$attributes$landuse$system)

  } else {

    # test that 'landuse' is a valid landuse system
    assertCharacter(x = landuse, len = 1)
    assertChoice(x = landuse, choices = opts$attributes$landuse$system)
    assertCharacter(x = layer, len = 1, any.missing = FALSE, null.ok = TRUE)

    assertNumeric(x = weight, finite = TRUE, any.missing = FALSE, null.ok = TRUE)
    if(!is.null(weight)){
      if(weight < -1 | weight > 1){
        warning("'weight' (", weight, ") should ideally be between -1 and 1, to fit the range of the probability maps.")
      }
    }

    assertSubset(x = layer, choices = c(opts$attributes$gridded$driver, "age", "sc1gr", "locspec", "cov"), empty.ok = TRUE)
    if(is.null(layer)){
      theLayer <- opts$files$cov_all.0.asc$obj
      theLayer[theLayer > 0] <- 0
      if(!is.null(weight)){
        layer <- "modifier"
      }
    } else {
      # test that the layer actually has correct values
      fileName <- opts$attributes$gridded$file[opts$attributes$gridded$driver == layer]
      theLayer <- opts$files[[fileName]]$obj
      theLayer <- setMinMax(theLayer)
      theLayer@file@nodatavalue <- -9999
      assertNumeric(x = range(values(theLayer)), lower = -1, upper = 1)
    }

    # and save it
    outName <- paste0("locspec", opts$attributes$landuse$ID[opts$attributes$landuse$system == landuse], ".fil.asc")
    opts$files[[outName]] <- list(obj = theLayer, type = "FLT4S")

    if(!is.null(layer)){
      gridID <- opts$attributes$landuse$ID[opts$attributes$landuse$system == landuse]
      for(i in 0:(opts$meta$last_year - opts$meta$first_year)){
        tempName <- paste0("locspec", gridID, ".", i, ".asc")
        opts$files[[tempName]] <- list(obj = theLayer, type = "FLT4S")
      }
    }

    if(is.null(weight)){
      weight <- 0
    }


    # update preference
    opts$attributes$landuse$preference[opts$attributes$landuse$system == landuse] <- weight

  }

  options(lclu = opts)
  invisible(landuse)

}
