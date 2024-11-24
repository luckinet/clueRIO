#' Set drivers of land systems
#'
#' @param scene description
#' @param file description
#' @param name description
#' @param ... additional arguments for for writing files with
#'   \link[terra]{writeRaster}.
#' @details
#' @return
#' @examples
#' @importFrom checkmate assertClass assertFileExists assertCharacter
#' @importFrom dplyr first last
#' @importFrom stringr str_split
#' @importFrom terra writeRaster
#' @export

clue_driver <- function(scene, file, name = NULL, ...){

  assertClass(x = scene, classes = "scene")
  assertCharacter(x = name, len = 1, any.missing = FALSE, null.ok = TRUE)
  isRast <- testClass(x = file, classes = "SpatRaster")

  root <- scene@meta$path

  # make sure it's a raster
  if(!isRast){
    assertFileExists(x = file, access = "rw")
    driver <- rast(file)
  } else {
    driver <- file
  }


  if(length(file) == 1){

    isDyn <- FALSE
    if(is.null(name)){
      name <- first(str_split(last(str_split(file, "/")[[1]]), "[.]")[[1]])
    }

  } else {

    isDyn <- TRUE
    if(is.null(name)){
      stop("please provide a name for this dynmic driver.")
    }

  }

  writeRaster(x = driver, filename = paste0(root, "input/", name, ".tif"),
              overwrite = TRUE, ...)

  temp <- list(dynamic = isDyn)

  scene@drivers[[name]] <- temp

  return(scene)
}
