#' Run the model
#'
#' @param scene description
#' @param mode description
#' @param stepwise description
#' @details
#' Additional details...
#' @examples
#' @importFrom checkmate assertClass
#' @export

clue_run <- function(scene, mode, stepwise){

  assertClass(x = scene, classes = "scene")

  root <- scene@meta$path

  # insert code from runModel.R here, check how much this could/should be in clue_initiate.R


}

