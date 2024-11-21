#' Validate the current status of a model
#'
#' This checks all directories and files and gives helpful feedback.
#' @param scene [`scene(1)`][scene]\cr the CLUE scene to validate.
#' @param verbose [`logical(1)`][logical]\cr whether or not to give the full
#'   diagnosis output, or just until the most recent error.
#' @details
#' @return no return value, called for the side-effect of checking the current
#'   setup.
#' @importFrom checkmate assertClass assertLogical
#' @export

clue_validate <- function(scene, verbose = FALSE){

  # custom function uesd only here
  .printOut <- function(x) {
    emptySpaces <- map(.x = seq_along(x), .f = \(ix) nchar(names(x)[ix])) |>
      unlist(use.names = FALSE) |>
      max(na.rm = TRUE)

    for (i in seq_along(x)) {
      thisLen <- nchar(names(x)[i])
      temp <- paste0(yellow(paste0(
        names(x)[i], paste0(rep(" ", emptySpaces - thisLen), collapse = ""), ": "
      )), x[i], "\n")
      print(temp)
    }
  }

  assertClass(x = scene, classes = "scene")
  assertLogical(x = verbose, len = 1, any.missing = FALSE)

  root <- getOption("clue_path")
  thisPath <- paste0(root, "/", scene@name)

  err <- list()

  # 1. test the working directory exists at all ----
  if(!testDirectory(x = thisPath, access = "rw")) {
    temp <- list("directory" = "missing")
  } else {
    temp <- list("directory" = "ok")

  }
  err <- c(err, temp)
  if (!verbose)
    .printOut(err)

  # # . ----
  # if(){
  #   temp <- list("" = "")
  # } else {
  #   temp <- list("" = "")
  #
  # }
  # err <- c(err, temp)
  # if(!verbose) .printOut(err)

  # test that 'landtypes$suitability$driver' is a valid driver
  # test that 'landtypes$production$crop' is a valid crop

  scene@validated <- TRUE

  return(scene)
}
