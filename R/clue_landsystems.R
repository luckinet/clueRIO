#' Set the initial land systems information
#'
#' @param scene description
#' @param file description
#' @param regions description
#' @param cats description
#' @details Additional details...
#' @return
#' @importFrom checkmate assertClass
#' @export

clue_landsystems <- function(scene, file, regions, cats = NULL){

  assertClass(x = scene, classes = "scene")


  # ensure this has a raster-attribute table: addCats()


  return(scene)
}
