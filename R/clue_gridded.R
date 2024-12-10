#' Set gridded layers
#'
#' @param scene [`scene(1)`][scene]\cr the CLUE scene to which to add a gridded
#'   layer.
#' @param file [`character(1)`][character]\cr path to a raster file that can be
#'   read with \link[terra]{rast}, or an item read in with that function.
#' @param type [`character(1)`][character]\cr type of the gridded layer. This
#'   determines where the layer can be used. Possible values are
#'   \code{"initial"}, \code{"restrictions"}, \code{"driver"},
#'   \code{"conversion"}, \code{"preference"} or \code{"kernel"}. See Details.
#' @param name [`character(1)`][character]\cr name of the layer as it will be
#'   called when setting up land types with \code{\link{clue_landtype}}.
#' @param ... additional arguments for for writing files with
#'   \link[terra]{writeRaster}.
#' @details
#' @return
#' @examples
#' @importFrom checkmate assertClass assertFileExists assertCharacter testClass
#'   assertChoice assertSubset
#' @importFrom dplyr first last
#' @importFrom stringr str_split str_replace
#' @importFrom purrr map_int
#' @importFrom terra writeRaster subst xmin ymin res ext nrow ncol datatype crs
#'   rast values set.crs
#' @export

clue_gridded <- function(scene, file, type, name = NULL, ...){

  assertClass(x = scene, classes = "scene")
  assertCharacter(x = name, len = 1, null.ok = TRUE)
  isRast <- testClass(x = file, classes = "SpatRaster")
  assertChoice(x = type, choices = c("initial", "restrictions", "driver", "conversion", "preference", "kernel"))

  root <- scene@meta$path
  opts <- getOption("clue")
  noData <- -9999

  .testGrid <- function(test, init, vals = NULL){

    assertFileExists(x = init, access = "rw")
    init <- rast(init)

    if(!is.null(vals)){
      # ensure the new layer has correct values
      xVals <- unique(values(test, mat = FALSE))
      assertSubset(x = xVals, choices = vals, .var.name = "vals(gridded)")
    }

    # ensure the new layer has the same attributes
    assertTRUE(x = all(res(init) == res(test)))
    assertTRUE(x = ext(init) == ext(test))
    assertTRUE(x = crs(init) == crs(test))

  }

  # make sure it's a raster
  if(!isRast){
    assertFileExists(x = file, access = "rw")
    gridded <- rast(file)

    # for test cases
    if(ext(gridded) == ext(0, 100, 0, 100) & all(res(gridded) == 1)){
      set.crs(x = gridded, "local")
    }
  } else {
    gridded <- file
  }

  # make sure the initial layer is defined before all else
  if(is.null(scene@grids$initial$file) & !type == "initial"){
    stop("please register the initial land systems map first; clue_gridded(..., type = 'initial')")
  }

  tbl_gridded <- map_dfr(.x = seq_along(scene@grids),
                         .f = function(ix){
                           bind_cols(name = names(scene@grids)[ix], bind_rows(scene@grids[[ix]]))
                         })

  if(type == "initial"){

    name <- "initial"
    fileName <- "cov_all.0"
    dataType <- "INT4U"
    theID <- 0

    # set options according to this layer
    opts$value[which(opts$var == "mapRows_n")] <- as.character(nrow(gridded))
    opts$value[which(opts$var == "mapCols_n")] <- as.character(ncol(gridded))
    opts$value[which(opts$var == "map_res")] <- as.character(res(gridded)[1])
    opts$value[which(opts$var == "map_xmin")] <- as.character(xmin(gridded))
    opts$value[which(opts$var == "map_ymin")] <- as.character(ymin(gridded))
    options(clue = opts)

  } else if(type == "restrictions"){

    name <- "restriction"
    fileName <- "region.fil"
    dataType <- "INT1U"
    theID <- 0

    # set required values
    gridded <- subst(gridded, NA, noData)
    gridded[gridded > 0] <- 0

    .testGrid(test = gridded,
              init = scene@grids$initial$file,
              vals = c(-9999, -9998, 0))

  } else if(type == "driver"){

    assertCharacter(x = name, any.missing = FALSE)
    fileName <- "sc1gr_X_.fil"
    dataType <- datatype(gridded)

    prev <- tbl_gridded |>
      filter(type %in% c("driver", "conversion"))

    if(dim(prev)[1] != 0){
      idSeq <- seq(min(prev$id), max(prev$id) + 1)
      theID <- min(idSeq[!idSeq %in% prev$id])
    } else {
      theID <- 0
    }

    fileName <- str_replace(string = fileName, pattern = "_X_", replacement = as.character(theID))

    # handle dynamic drivers
    if(length(file) != 1){
      stop("dynamic drivers are currently work in progress")
      # fileName <- str_replace(string = fileName, pattern = "fil", replacement = year values)

    }

    .testGrid(test = gridded,
              init = scene@grids$initial$file)

  } else if(type == "conversion"){

    assertCharacter(x = name, any.missing = FALSE)
    fileName <- "sc1gr_X_.fil"
    dataType <- datatype(gridded)
    noData <- -9999

    prev <- tbl_gridded |>
      filter(type %in% c("driver", "conversion"))

    if(dim(prev)[1] %in% c(0, 1)){
      theID <- 2
    } else {
      theID <- max(prev$id) + 1
    }

    fileName <- str_replace(string = fileName, pattern = "_X_", replacement = as.character(theID))

    .testGrid(test = gridded,
              init = scene@grids$initial$file)

  } else if(type == "preference"){

    assertCharacter(x = name, any.missing = FALSE)
    fileName <- paste0("locspec_", name, "_.fil")
    dataType <- "INT1U"
    theID <- NA_integer_

    # set required values
    gridded <- subst(gridded, NA, noData)
    gridded[gridded > 1] <- 1

    # handle dynamic drivers
    if(length(file) != 1){
      stop("dynamic location specific preferences are currently work in progress")
      # fileName <- str_replace(string = fileName, pattern = "fil", replacement = year values)

    }

    .testGrid(test = gridded,
              init = scene@grids$initial$file,
              vals = c(-9999, 0, 1))

  } else if(type == "kernel"){
    name <- "kernel"
    dataType <- "FLT4S"

    gridded <- subst(gridded, NA, 0)
    prev <- tbl_gridded |>
      filter(type %in% c("kernel"))

    theID <- dim(prev)[1] + 1
    fileName <- paste0("kernel", theID)

  }

  # store parameters ----
  temp <- list(file = paste0(root, fileName, ".asc"),
               type = type,
               id = theID)

  scene@grids[[name]] <- temp

  # save item ----
  writeRaster(x = gridded,
              filename = paste0(root, fileName, ".asc"),
              overwrite = TRUE,
              datatype = dataType,
              NAflag = noData)

  return(scene)
}
