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

  root <- getOption("clue_path")
  thisPath <- paste0(root, "/", scene@name)



}

