#' Prepare the CLUE model directory
#'
#' This creates directories and initial files from a CLUE scene.
#' @param scene [`scene(1)`][scene]\cr the CLUE scene for which to prepare the
#'   model directory.
#' @param clue.exe [`character(1)`][character]\cr by default, this package ships
#'   with the publicly available version of CLUMondo. Optionally, you can
#'   specify here the full path to another \emph{clue.exe} file with which the
#'   model should be run.
#' @details Additional details...
#' @return no return value, called for the side-effect of creating directories
#'   and initial files.
#' @importFrom checkmate assertClass
#' @export

clue_initiate <- function(scene, clue.exe = NULL){

  assertClass(x = scene, classes = "scene")
  assertFileExists(x = clue.exe, access = "r", extension = "exe")

  assertTRUE(x = scene@valid)

  root <- getOption("clue_path")
  thisPath <- paste0(root, "/", scene@name)

  # create directories ----
  if (!testDirectory(x = thisPath, access = "rw")) {
    dir.create(thisPath, recursive = TRUE)
    dir.create(file.path(thisPath, "backup"))
    dir.create(file.path(thisPath, "input"))
    dir.create(file.path(thisPath, "output"))
    if(is.null(clue.exe)){
      file.copy(from = system.file("CLUMondo.exe", package = "clueRIO", mustWork = TRUE),
                to = paste0(thisPath, "/CLUMondo.exe"))
    } else {
      file.copy(from = clue.exe,
                to = paste0(thisPath, "/CLUMondo.exe"))
    }
  }

  # ----



  return(scene)
}
