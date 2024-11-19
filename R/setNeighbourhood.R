#' Specify neighbourhood models
#'
#' This function sets the parameters for neigbourhood modelling. This requires
#' to specify the neighbourhood (dimensions and weights) and a regression
#' equation that determines how neighborhood results contribute to the
#' suitability of a location.
#' @param landuse [`character(1)`][character]\cr the landuse type for which to
#'   specify the neighbourhood parameters.
#' @param const [`numeric(1)`][numeric]\cr constant of regression equation (how
#'   neighborhood results contribute to the suitability of a location) for this
#'   landuse type.
#' @param ... [`numeric(1)`][numeric]\cr named value, where the name is the
#'   landuse type in the neighbourhood and the value is the respective beta
#'   coefficient of the regression equation (how neighborhood results contribute
#'   to the suitability of a location).
#' @param weight [`numeric(1)`][numeric]\cr the weight (0 - 1) assigned to the
#'   neighborhood function.
#' @param kernel [`matrix(1)`][matrix]\cr the region within which neighbourhood
#'   shall be considered. This is a matrix with an odd number of columns and
#'   rows containing weights (0 - 1) of the cells in the neighbourhood.
#' @param table [`data.frame(1)`][data.frame]\cr object that contains one or
#'   more landuse types in rows and the columns \code{"landuse"} and
#'   \code{"const"} of each type and any number of landuse types with the
#'   respective model coefficients. (not yet implemented)
#' @details When a land use type is not influenced by a neighborhood function
#'   (while others are), \code{const} is set to 0 and nothing else is specified.
#' @family functions to specify landuse
#' @importFrom checkmate assertTibble assertNumeric assertCharacter assertNames
#'   assertChoice assertSubset assertIntegerish assertMatrix
#' @importFrom rlang exprs
#' @export

setNeighbourhood <- function(landuse = NULL, const = NULL, ..., weight = NULL,
                             kernel = matrix(1, 3, 3), table = NULL){

  opts <- getOption("lclu")
  neigh <- opts$tables$alloc2

  assertTibble(x = table, min.cols = 3, nrows = length(opts$attributes$landuse$system), null.ok = TRUE)
  if(!is.null(table)){

  } else {

    # test that 'landuse' is a valid landuse system
    assertChoice(x = landuse, choices = opts$attributes$landuse$system)
    assertCharacter(x = landuse, len = 1)
    assertNumeric(x = const, len = 1, finite = TRUE, null.ok = TRUE)

    if(!is.null(const)){

      temp <- exprs(..., .named = TRUE)
      assertNumeric(x = weight, len = 1, finite = TRUE, lower = 0, upper = 1)
      assertMatrix(x = kernel, min.cols = 3, min.rows = 3, any.missing = FALSE)
      assertTRUE(x = dim(kernel)[1] == dim(kernel)[2])
      assertNumeric(x = kernel, lower = 0, upper = 1)

      # test that the specified landuse systems are valid landuse systems
      assertSubset(x = names(temp), choices = opts$attributes$landuse$system)

      newDrivers <- names(temp)
      assertNames(newDrivers, disjunct.from = "const")

      for(j in seq_along(newDrivers)){
        if(!any(names(neigh) %in% newDrivers[j])){
          neigh[newDrivers[j]] <- 0
        }
        neigh[newDrivers[j]][which(neigh$system == landuse),] <- temp[[j]]
      }

      params <- sapply(seq_along(temp), function(x){
        betaPos <- which(opts$attributes$landuse$system %in% names(temp)[x]) - 1
        assertIntegerish(x = betaPos, len = 1)
        paste0(temp[[x]], " ", betaPos)
      })

      params <- paste0(paste(params, collapse = "\n"), "\n")

    } else {
      const <- 0
      weight <- 0
      params <- NULL
      temp <- NULL
    }

    if(length(temp) > 0){
      outKernel <- sapply(1:dim(kernel)[1], function(x){
        paste0(kernel[x, ], collapse = " ")
      })
      outNeigh <- paste0(length(temp), " ", paste0(opts$attributes$landuse$ID[opts$attributes$landuse$system %in% landuse], collapse = " "), "\n\n",
                         (dim(kernel)[1]-1) / 2, "\n", paste0(outKernel, collapse = "\n"), "\n")
    } else {
      outNeigh <- ""
    }

    newAlloc <- paste0(opts$attributes$landuse$ID[opts$attributes$landuse$system == landuse], "\n", # Number code for land use type
                       const, "\n", # Constant of neighborhood regression equation for land use type (ß0)
                       length(temp), "\n", # Number of explanatory factors (land use types) in the equation for that land use type (for all these the weight - set in neighmat.txt - should be > 0 otherwise the neighborhood is not calculated)
                       params) # On each line the coefficients for the explanatory factors and the number code of the explanatory factor. In using the neighborhood function, the explanatory factors are the land use types and therefore the number codes are the number codes for the land use types.

    newNeighmat <- paste0(outNeigh)

    neigh["const"][which(neigh$system == landuse),] <- const
    neigh["weight"][which(neigh$system == landuse),] <- weight
    neigh["alloc"][which(neigh$system == landuse),] <- newAlloc
    neigh["neighmat"][which(neigh$system == landuse),] <- newNeighmat
  }

  opts$tables$main["value"][which(opts$tables$main$var == "neigh"),] <- as.character(1)

  opts$tables$alloc2 <- neigh

  options(lclu = opts)

  invisible(landuse)
}