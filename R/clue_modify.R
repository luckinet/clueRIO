#' Apply a modifier to a CLUE scene
#'
#' @param scene [`scene(1)`][scene]\cr the CLUE scene to mofidy.
#' @details
#' @examples
#' @return no return value, called for the side effect of ...
#' @importFrom checkmate assertClass
#' @export

clue_modify <- function(scene){

  assertClass(x = scene, classes = "scene")


  # stepwise

  # self-limit (see setPreference.R in the old functions)

}
