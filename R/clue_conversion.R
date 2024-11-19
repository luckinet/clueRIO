#' Set a conversion rule
#'
#' This function constructs the conversion between from one to the other land
#' type.
#' @param scene description
#' @param from [`character(1)`][character]\cr a land type \emph{from} which a
#'   conversion shall be created.
#' @param to [`character(1)`][character]\cr a land type \emph{to} which the
#'   conversion shall be created.
#' @param label [`character(1)`][character]\cr description of the conversion
#'   (used in the sequence plot).
#' @param thresh [`integerish(1)`][integer]\cr the number of time steps the land
#'   type in \code{from} must remain the same before it can change into the land
#'   type in \code{to}.
#' @param auto [`integerish(1)`][integer]\cr the number of time steps after
#'   which the land type in \code{from} will automatically change into
#'   \code{to}.
#' @param mask [`character(1)`][character]\cr the name of a driver (see
#'   \code{\link{clue_driver}}) for which conversion to the specified class is
#'   allowed.
#' @details
#' @return
#' @importFrom checkmate assertClass
#' @export

clue_conversion <- function(scene, from, to, label = NULL, thresh = NULL,
                            auto = NULL, mask = NULL){

  assertClass(x = scene, classes = "scene")



  return(scene)
}
