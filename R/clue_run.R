#' Run the model
#'
#' @param scene [`scene(1)`][scene]\cr the CLUE scene for which to run the
#'   model.
#' @param clue.exe [`character(1)`][character]\cr R packages cannot contain
#'   executable files, such as the CLUMondo main model executable. Therefore,
#'   you need to download this
#'   \href{https://www.environmentalgeography.nl/site/data-models/models/clumondo-model/}{CLUMondo*.exe}
#'   from the official model website and provide the path to your local copy
#'   here.
#' @param mode [`integerish(1)`][integer]\cr the model mode.
#' @details Additional details...
#' @examples
#' @details
#' @return no return value, called for the side-effect of calling \code{scene}
#'   and running the CLUMondo model.
#' @importFrom checkmate assertClass
#' @export

clue_run <- function(scene, clue.exe, mode){

  assertClass(x = scene, classes = "scene")
  assertFileExists(x = clue.exe, access = "r", extension = "exe")

  root <- scene@meta$path

  # copy executable to the model directory ----
  file.copy(from = clue.exe, to = paste0(root, "CLUMondo.exe"))

  # set model mode ----
  if(!is.null(mode)){
    message("  --> running the model with mode ", mode)
    model_mode <- paste0(" ", mode)
  } else {
    model_mode <- NULL
  }


  # run the model ----
  message("\nrunning CLUMondo")
  # message("\nrunning CLUMondo for the years ", theYears[1], " - ", theYears[2], ".")
  if(Sys.info()[['sysname']] == "Linux"){
    system(paste0("cd ", root, "\n wine CLUMondo.exe demand.in1 region.fil.asc", model_mode))
  } else {
    system(paste0("cd ", root, "\n CLUMondo.exe demand.in1 region.fil.asc", model_mode))
  }

  # push data to output ----
  output <- list.files(path = paste0(root), pattern = "age|cov_all|landarea|outputmatrix")
  file.copy(from = paste0(root, "/", output),
            to = paste0(root, "/output/", output),
            overwrite = TRUE)

  # for stepwise
  # if(i == 1){
  #   first <- list.files(path = paste0(opts$path$model), pattern = paste0("cov_all.0.asc|age.0.asc"))
  #   file.copy(from = paste0(opts$path$model, "/", first),
  #             to = paste0(opts$path$model, "/output/", first),
  #             overwrite = TRUE)
  # }
  # output <- list.files(path = paste0(opts$path$model), pattern = paste0("cov_all.1.asc|age.1.asc"))
  # outNames <- c(paste0("age.", i, ".asc"), paste0("cov_all.", i, ".asc"))
  # file.copy(from = paste0(opts$path$model, "/", output),
  #           to = paste0(opts$path$model, "/output/", outNames),
  #           overwrite = TRUE)


}

