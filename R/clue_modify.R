#' Apply a modifier to a CLUE scene
#'
#' @param scene description
#' @details
#' @examples
#' @return no return value, called for the side effect of ...
#' @importFrom checkmate assertClass
#' @export

clue_modify <- function(scene){

  assertClass(x = scene, classes = "scene")

}
