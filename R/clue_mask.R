#' Set a mask to spatially restrict a model
#'
#' @param scene description
#' @param mask description
#' @details
#' @return
#' @examples
#' @importFrom checkmate assertClass testClass assertFileExists
#' @importFrom sf write_sf
#' @importFrom terra rast writeRaster res ext crs
#' @export

clue_mask <- function(scene, file){

  assertClass(x = scene, classes = "scene")
  isRast <- testClass(x = file, classes = "SpatRaster")

  root <- file.path(scene@meta$path)

  assertFileExists(x = paste0(root, "input/landsystems.tif"), access = "rw")
  map <- rast(x = paste0(root, "input/landsystems.tif"))

  if(!isRast){
    assertFileExists(x = file, access = "rw")
    mask <- rast(file)
  } else {
    mask <- file
  }

  # ensure the mask has correct values
  maskVals <- unique(values(mask, mat = FALSE))
  assertTRUE(x = all(c(0, 1) %in% maskVals))

  # ensure that mask has the same attributes as map
  assertTRUE(x = all(res(map) == res(mask)))
  assertTRUE(x = ext(map) == ext(mask))
  assertTRUE(x = crs(map) == crs(mask))

  writeRaster(x = mask, filename = paste0(root, "input/mask.tif"),
              overwrite = TRUE,
              filetype = "GTiff",
              datatype = "INT1U",
              gdal = c("COMPRESS=DEFLATE", "ZLEVEL=9", "PREDICTOR=2"))

  return(scene)
}
