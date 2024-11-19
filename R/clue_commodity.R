#' Set commodities and their attributes
#'
#' @param scene description
#' @param name [`character(1)`][character]\cr name of the commodity.
#' @param demand [`data.frame(1)`][data.frame]\cr table of when, where and how
#'   much of the commodity is produced. Must have names \code{year},
#'   \code{region} and \code{amount}.
#' @details Additional details...
#' @return
#' @importFrom checkmate assertClass
#' @export

clue_commodity <- function(scene, name, demand = NULL){

  assertClass(x = scene, classes = "scene")
  assertCharacter(x = name, len = 1, any.missing = FALSE)
  assertDataFrame(x = demand, ncols = 3, all.missing = FALSE, null.ok = TRUE)
  if(!is.null(demand)){
    assertNames(x = names(demand), permutation.of = c("year", "region", "amount"))
  }


  return(scene)
}
