#' Set gridded objects
#'
#' This function makes an inventory of driving factors by recording the name and
#' exact path of each gridded layer that shall be used as driving factor.
#' @param name [`character(1)`][character]\cr the name of the driving factor.
#' @param dir [`character(1)`][character]\cr path of the directory where to find the
#'   layers of this driver.
#' @param pattern [`character(1)`][character]\cr a regular expression to detect all
#'   files that are part of that driving factor.
#' @details If there is more than one layer selected via \code{pattern}, the
#'   function makes sure that for this driver there is a layer for each year
#'   that has been defined by \code{\link{newProject}}.
#'
#'   \code{name = "landuse"} is reserved to register the mandatory intial
#'   landuse layer and \code{name = "region"} for the optional area under
#'   consideration. \code{"landuse"} must be provided with an attribute table
#'   that contains all the landuse systems that shall be modelled (see
#'   \code{\link{setLAT}}).
#' @family functions to register items
#' @importFrom checkmate assertCharacter assertDirectory assertDataFrame
#'   assertNames assertTRUE
#' @importFrom raster raster stack setMinMax extent res crs crs<- xmin
#'   ymin resample mask writeRaster values
#' @importFrom dplyr add_row
#' @export

regGridded <- function(name = NULL, dir = NULL, pattern = NULL, crs = NULL,
                       obj = NULL, table = NULL, ...){

  .Deprecated("clue_driver")

  opts <- getOption("lclu")

  assertCharacter(x = name, any.missing = FALSE, len = 1)
  assertClass(x = obj, classes = "RasterLayer", null.ok = TRUE)
  assertDataFrame(x = table, any.missing = FALSE, null.ok = TRUE)

  if (!is.null(table)) {
    assertNames(x = colnames(table), must.include = c("name", "dir", "pattern"))
    name <- table$name
    dir <- table$dir
    pattern <- table$pattern
  } else if(is.null(obj)) {
    assertCharacter(x = dir, any.missing = FALSE, len = 1)
    assertCharacter(x = crs, null.ok = TRUE)
  }

  if(name %in% opts$attributes$gridded$driver){
    stop("the driver '", name, "' has already been registered.")
  }

  for(i in seq_along(name)){

    if(!is.null(obj)){
      theName <- name
      theRaster <- obj
      theRaster <- setMinMax(theRaster)

      if(length(theRaster@data@attributes) != 0){
        theRAT <- theRaster@data@attributes[[1]]
      } else {
        theRAT <- NULL
      }
    } else {
      theDir <- dir[i]
      theName <- name[i]
      assertDirectory(x = theDir, access = "r")
      assertCharacter(x = pattern, any.missing = FALSE, len = 1, null.ok = TRUE)
      thePath <- list.files(path = theDir, pattern = pattern, full.names = T)
      thePath <- thePath[!grepl(pattern = "aux.xml", x = thePath)]
      if(length(thePath) > 1) {

        # test that number of layers corresponds to number of years
        nrYears <- length(seq(opts$meta$first_year, opts$meta$last_year))
        assertTRUE(length(thePath) == nrYears, .var.name = "nrLayers == nrYears")

        message("  --> registering ", length(thePath), " layers ...")
        theRaster <- stack(thePath)
        theRAT <- NULL

      } else {
        theRaster <- raster(thePath)
        theRaster <- setMinMax(theRaster)

        if(length(theRaster@data@attributes) != 0){
          theRAT <- theRaster@data@attributes[[1]]
        } else {
          theRAT <- NULL
        }
      }
    }

    if(is.na(crs(theRaster))) {
      if(is.null(crs)) {
        stop("please specify a coordinate reference system with 'crs = ...' for this object.")
      } else {
        crs(theRaster) <- crs
      }
    }
    theRes <- res(theRaster)
    theExtent <- extent(theRaster)
    theDims <- dim(theRaster)
    if(any(theDims[1] > 4000 | theDims[2] > 4000)) {
      stop("this gridded object is too large!")
    }

    if(theName == "landuse"){

      # test that values are only integer
      theCells <- table(values(theRaster))
      cellValues <- as.numeric(names(theCells))

      assertIntegerish(x = cellValues)

      if(is.null(theRAT)){
        warning("the landuse grid does not contain an attribute table, please make sure to set one via 'setLAT()'.")
      } else {
        break # implement once I encounter it
        # test that the attribute table contains all the landuse types defined
        # at the beginning of the project and extract it to opts$attributes$landuse
        # opts$attributes$landuse <- theRAT
      }

      names(theRaster) <- "cov_all.0"
      tempType <- "INT1U"
      opts$meta$cells <- length(values(theRaster))
      # in case no project resolution or extent has been set, take it from initial landuse
      if(is.null(opts$meta$resolution)){
        opts$meta$resolution <- theRes
      }
      if(is.null(opts$meta$extent)){
        opts$meta$extent <- theExtent
      }
      target <- raster(opts$meta$extent, res = opts$meta$resolution)
      # derive options
      opts$tables$main["value"][which(opts$tables$main$var == "mapRows_n"),] <- as.character(nrow(theRaster))
      opts$tables$main["value"][which(opts$tables$main$var == "mapCols_n"),] <- as.character(ncol(theRaster))
      opts$tables$main["value"][which(opts$tables$main$var == "map_res"),] <- as.character(res(theRaster)[1])
      opts$tables$main["value"][which(opts$tables$main$var == "map_xmin"),] <- as.character(xmin(theRaster))
      opts$tables$main["value"][which(opts$tables$main$var == "map_ymin"),] <- as.character(ymin(theRaster))

    } else{

      if(theName == "region"){
        # ensure here that 'region' is properly specified
        names(theRaster) <- "region.fil"
        tempType <- "INT2S"
        tempDyn <- FALSE
        docName <- "region.fil.asc"
      } else {

        items <- dim(theRaster)[3]
        prevGrids <- length(opts$attributes$gridded$driver)

        if(items > 1){
          tempName <- paste0("sc1gr", prevGrids, ".", (1:items)-1)
          tempDyn <- TRUE
        } else {
          tempName <- paste0("sc1gr", prevGrids, ".fil")
          tempDyn <- FALSE
        }

        if(items > 1){
          if(is.na(opts$tables$main[["value"]][which(opts$tables$main$var == "drivers_dyn")])){
            newDyn <- prevGrids
          } else {
            newDyn <- c(opts$tables$main[["value"]][which(opts$tables$main$var == "drivers_dyn")], prevGrids)
          }
          opts$tables$main["value"][which(opts$tables$main$var == "drivers_dyn"),] <- as.character(newDyn)
        }

        names(theRaster) <- tempName
        docName <- paste0("sc1gr", prevGrids, ".fil.asc")
        tempType <- "FLT4S"

      }
      target <- opts$files$cov_all.0.asc$obj
      opts$attributes$gridded <- add_row(.data = opts$attributes$gridded,
                                         driver = theName,
                                         file = docName,
                                         dynamic = tempDyn)
    }

    # go through all rasters in theRaster to adapt and save them
    for(j in 1:dim(theRaster)[3]){

      # test and adapt resolution
      if(!all(theRes == opts$meta$resolution)){
        message("      -> resampling to target resolution '", paste(theRes[1], theRes[2], collapse = ", "), "'")
        theRaster <- resample(x = theRaster, y = target, method = "ngb")
      }

      # test and adapt extent
      if(any(theExtent@xmin >= opts$extent[1] |
             theExtent@xmax <= opts$extent[2] |
             theExtent@ymin >= opts$extent[3] |
             theExtent@ymax <= opts$extent[4])){
        stop("the gridded object does not fully cover the focal region.")
      }

      if(!theExtent == opts$meta$extent){
        message("      -> masking to target extent '", paste(theExtent@xmin, theExtent@xmax, theExtent@ymin, theExtent@ymax, collapse = ", "), "'")
        theRaster <- mask(x = theRaster, mask = target)
      }

      outRaster <- theRaster[[j]]
      outName <- paste0(names(theRaster)[j], ".asc")
      outRaster@file@nodatavalue <- -9999
      opts$files[[outName]] <- list(obj = outRaster, type = tempType)

      if(dim(theRaster)[3] > 1 & j == 1){
        opts$files[[docName]] <- list(obj = outRaster, type = tempType)
      }
    }

  }

  options(lclu = opts)
  invisible(name)
}
