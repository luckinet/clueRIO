#' Build an example CLUE scene
#'
#' This function helps setting up an example scene with custom properties.
#' @param path [`character(1)`][character]\cr The scene has by default the root
#'   directory tempdir(), but if you want it in a particular location, specify
#'   that in this argument.
#' @param until [`character(1)`][character]\cr The step until which the scene
#'   shall be built, one of \code{...},
#' @param verbose [`logical(1)`][logical]\cr be verbose about building the
#'   example scene (default \code{FALSE}).
#' @return No return value, called for the side effect of creating an example
#'   scene at the specified \code{path}.
#' @examples
#' if(dev.interactive()){
#' # to build the full example database
#' clue_example(path = paste0(tempdir(), "/myScene"))
#'
#' # to make the example database until a certain step
#' clue_example(path = paste0(tempdir(), "/myScene"), until = "")
#'
#' }
#' @importFrom checkmate assertChoice testDirectoryExists
#' @export

clue_example <- function(path = NULL, until = NULL, verbose = FALSE){

  # set internal paths
  inPath <- system.file("test_datasets", package = "clueRIO", mustWork = TRUE)
  steps <- c("")



}
