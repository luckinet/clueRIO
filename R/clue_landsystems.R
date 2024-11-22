#' Set the initial land systems information
#'
#' @param scene description
#' @param map description
#' @param cats (mention that either to provide this, or set it already before in
#'   \code{map}).
#' @param ... additional arguments for for writing files with
#'   \link[terra]{writeRaster}.
#' @details
#' @return
#' @examples
#' @importFrom checkmate assertClass testClass assertFileExists testNull
#'   assertTRUE
#' @importFrom sf write_sf
#' @importFrom terra rast set.cats levels writeRaster
#' @export

clue_landsystems <- function(scene, file, cats = NULL, ...){

  assertClass(x = scene, classes = "scene")
  isRast <- testClass(x = file, classes = "SpatRaster")

  root <- file.path(scene@meta$path)

  # make sure it's a raster
  if(!isRast){
    assertFileExists(x = file, access = "rw")
    map <- rast(file)
  } else {
    map <- file
  }

  if(is.null(cats(map)[[1]])){
    if(is.null(cats)){
      stop("please ensure that 'map' has a valid category table - see 'terra::cats()' - or provide it via the argument 'cats = ...'.")
    } else {
      assertDataFrame(x = cats, types = c("integerish", "character"))

      # ensure that cats has the correct values
      mapVals <- unique(values(map, mat = FALSE))
      assertTRUE(x = all(cats$ID %in% mapVals))
      assertTRUE(x = all(mapVals %in% cats$ID))

      set.cats(x = map, value = cats)
    }
  }

  scene@landsystems <- levels(map)[[1]]

  # set options according to this layer
  opts <- getOption("clue")
  opts$value[which(opts$var == "mapRows_n")] <- as.character(nrow(map))
  opts$value[which(opts$var == "mapCols_n")] <- as.character(ncol(map))
  opts$value[which(opts$var == "map_res")] <- as.character(res(map))
  opts$value[which(opts$var == "map_xmin")] <- as.character(xmin(map))
  opts$value[which(opts$var == "map_ymin")] <- as.character(ymin(map))
  options(clue = opts)

  writeRaster(x = map, filename = paste0(root, "input/landsystems.tif"), overwrite = TRUE, ...)

  return(scene)
}
