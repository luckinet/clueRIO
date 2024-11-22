#' Specify suitability models
#'
#' This function sets the parameters for suitability modelling.
#' @param landuse [`character(1)`][character]\cr the landuse type for which to
#'   specify suitability parameters.
#' @param const [`numeric(1)`][numeric]\cr constant of regression equation for
#'   this landuse type.
#' @param ... named value, where the name is the
#'   gridded object name and the value is the beta coefficient of the regression
#'   equation.
#' @param table [`data.frame(1)`][data.frame]\cr object that contains one or
#'   more land-use types in rows and the columns \code{"landuse"} and
#'   \code{"const"} of each type and any number of explanatory factors with the
#'   respective model coefficients. Explanatory factor names must correspond to
#'   the drivers set in \code{\link{setDriver}}
#' @family functions to specify landuse
#' @importFrom checkmate assertTibble assertNumeric assertCharacter assertNames
#'   assertChoice assertSubset assertIntegerish
#' @importFrom tibble add_column
#' @importFrom dplyr select
#' @importFrom rlang exprs
#' @export

setSuitability <- function(landuse = NULL, const = NULL, ..., table = NULL){

  .Deprecated("clue_landtype")

  opts <- getOption("lclu")

  assertTibble(x = table, min.cols = 3, nrows = length(opts$attributes$landuse$system), null.ok = TRUE)
  if(!is.null(table)){
    assertNames(x = names(table), must.include = c("landuse", "const"))
    assertNames(x = table$landuse, permutation.of = opts$attributes$landuse$system)
    temp <- table %>%
      dplyr::select(-landuse, -const)
    elements <- sapply(seq_along(table$landuse), function(x){
      dim(temp[which(temp[x,] != 0)])[2]
    })
    params <- sapply(seq_along(table$landuse), function(x){
      pos <- which(temp[x,] != 0) - 1
      beta <- temp[x,][pos+1]
      paste(paste0(beta, " ", pos), collapse = "\n\t")
    })
    theString <- paste0(opts$attributes$landuse$ID[opts$attributes$landuse$system == table$landuse], "\n\t",
                        table$const, "\n",
                        elements, "\n\t",
                        params, "\n")
    suit <- table %>%
      add_column(alloc = theString) %>%
      dplyr::select(system = landuse, alloc, everything())

    landuse <- table$landuse
    newDriversN <- dim(table)[2] - 2
  } else {
    assertNumeric(x = const, len = 1, finite = TRUE)
    assertCharacter(x = landuse, len = 1)
    temp <- exprs(..., .named = TRUE)
    suit <- opts$tables$alloc1

    # test that 'landuse' is a valid landuse system
    assertChoice(x = landuse, choices = opts$attributes$landuse$system)
    # test that the specified drivers are known
    assertSubset(x = names(temp), choices = opts$attributes$gridded$driver)

    suit["const"][which(suit$system == landuse),] <- const

    newDrivers <- names(temp)
    assertNames(newDrivers, disjunct.from = "const")

    for(j in seq_along(newDrivers)){
      if(!any(names(suit) %in% newDrivers[j])){
        suit[newDrivers[j]] <- 0
      }
      suit[newDrivers[j]][which(suit$system == landuse),] <- temp[[j]]
    }

    params <- sapply(seq_along(temp), function(x){
      betaPos <- which(opts$attributes$gridded$driver %in% names(temp)[x]) - 1
      assertIntegerish(x = betaPos, len = 1)
      paste0(temp[[x]], " ", betaPos)
    })
    newString <- paste0(opts$attributes$landuse$ID[opts$attributes$landuse$system == landuse], "\n\t", # Number code for land use type
                        const, "\n", # Constant of regression equation for land use type (ß0)
                        length(temp), "\n\t", # Number of explanatory factors (sc1gr#.fil files) in the regression equation for that land use type. For example population density, distance to roads and volcanic rocks are the explanatory factors for the land use type forest, so the number of explanatory factors is 3.
                        paste(params, collapse = "\n\t"), "\n") # On each line the beta coefficients (ß1, ß2, etc.) for the explanatory factor and the number code of the explanatory factor.

    suit["alloc"][which(suit$system == landuse),] <- newString
    newDriversN <- 1
  }

  # derive options
  opts$tables$main["value"][which(opts$tables$main$var == "drivers_max"),] <-
    as.character(as.numeric(opts$tables$main["value"][which(opts$tables$main$var == "drivers_max"),]) + newDriversN)
  opts$tables$main["value"][which(opts$tables$main$var == "drivers_tot"),] <-
    as.character(as.numeric(opts$tables$main["value"][which(opts$tables$main$var == "drivers_tot"),]) + newDriversN)

  opts$tables$alloc1 <- suit

  options(lclu = opts)

  invisible(landuse)
}
