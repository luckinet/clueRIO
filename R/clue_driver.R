#' Set drivers of land systems
#'
#' @param scene description
#' @param file description
#' @param name description
#' @details Additional details...
#' @return
#' @importFrom checkmate assertClass
#' @export

clue_driver <- function(scene, file, name = NULL){

  assertClass(x = scene, classes = "scene")


  return(scene)
}
