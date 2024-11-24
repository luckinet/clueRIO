#' Prepare the CLUE model directory
#'
#' This creates directories and initial files from a CLUE scene.
#' @param scene [`scene(1)`][scene]\cr the CLUE scene for which to prepare the
#'   model directory.
#' @param clue.exe [`character(1)`][character]\cr R packages cannot contain
#'   executable files, such as the CLUMondo main model executable. Therefore,
#'   you need to download this
#'   \href{https://www.environmentalgeography.nl/site/data-models/models/clumondo-model/}{CLUMondo*.exe}
#'   from the official model website and provide the path to your local copy
#'   here.
#' @param modify description
#' @details
#' @return no return value, called for the side-effect of creating directories
#'   and initial files according to the model scene.
#' @importFrom checkmate assertClass assertFileExists assertTRUE assertChoice
#' @export

clue_initiate <- function(scene, clue.exe, modify = NULL){

  assertClass(x = scene, classes = "scene")
  assertFileExists(x = clue.exe, access = "r", extension = "exe")
  # assertChoice(x = modify, choices = c(), null.ok = TRUE)

  assertTRUE(x = scene@validated)

  # model directory
  root <- scene@meta$path

  # copy executable to the model directory ----
  file.copy(from = clue.exe, to = paste0(root, "CLUMondo.exe"))

  # save landsystems.tif as cov_all.0 ----
  # if(length(list.files(wd, pattern = "cov_all.0")) == 0) {
  #   message("please create a gridded layer of the initial landuse with 'regGridded(name = \"landuse\", ...)'")
  # }


  # insert code from makeModel.R here
  # insert code from testModel.R here

  # - ensure that 'landtypes$suitability$driver' is a valid driver
  # - ensure that 'landtypes$production$crop' is a valid crop
  # - test for each commodity that demand$year has the same years as defined in the meta data and report missing years or filter years not in the meta data and report this
  # - test for each commodity that demand$region has the same values as defined in the slot regions and report missing regions or filter regions not in the slot and report this
  #
  #
  # - test dimensions of rasters perhaps
  # theDims <- dim(theRaster)
  # if(any(theDims[1] > 4000 | theDims[2] > 4000)) {
  #   stop("this gridded object is too large!")
  # }
  # - test the crs and extent as well

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
  list("directory" = "ok") |>
    .printOut()


  # newlLus <- newAllow <- newTypes <- newSuit <- newNeigh <- tibble(system = landuse)
  # newlLus[,demands] <- 0
  # newAllow[,landuse] <- 0
  # newTypes[,landuse] <- NA_character_
  # newSuit$alloc <- NA_character_
  # newSuit$const <- NA_real_
  # newNeigh$alloc <- NA_character_
  # newNeigh$neighmat <- NA_character_
  # newNeigh$const <- NA_real_
  # newNeigh$weight <- 0
  #
  #
  # opts <- list(tables = list(main = tibble(var = c("ls_types_n", "regions_n", "drivers_max", "drivers_tot",
  #                                                  "demand_types_n", "mapRows_n", "mapCols_n", "map_res",
  #                                                  "map_xmin", "map_ymin", "ls_types", "resistance",
  #                                                  "demand_type", "iter_vars", "years", "drivers_dyn",
  #                                                  "out_type", "rs_reg", "ls_hist", "neigh", "loc_pref",
  #                                                  "dyn_lusmat", "out_write", "iter_param"),
  #                                          value = c(NA, 1, 1, 0, NA, NA,
  #                                                    NA, NA, NA, NA, NA, NA,
  #                                                    NA, paste(c(0, 0.5, 1), collapse = "\t"), paste(c(start, end), collapse = "\t"), NA, 3, 0,
  #                                                    paste(c(1, 5), collapse = "\t"), 0, 0, 0, 1, 0.05)),
  #                            alloc1 = newSuit,
  #                            alloc2 = newNeigh,
  #                            allow = newAllow,
  #                            lusconv = newlLus,
  #                            lusmatrix = newlLus,
  #                            allowTypes = newTypes),
  #              data = list(regions = NULL,
  #                          demand = NULL),
  #              files = list(),
  #              modifier = NULL)


  # which files need to be initiated?
  # alloc1.reg
  # writeLines(outAlloc1, paste0(root, "alloc1.reg"))
  # alloc2.reg
  # writeLines(outAlloc2, paste0(root, "alloc2.reg"))
  # neighmat.txt
  # writeLines(outNeighmat, paste0(root, "neighmat.txt"))
  # allow.txt
  # write_delim(x = as_tibble(outAllow, .name_repair = "minimal"), path = paste0(root, "allow.txt"), col_names = FALSE)
  # lusconv.txt
  # write_delim(x = as_tibble(outConv, .name_repair = "minimal"), path = paste0(root, "lusconv.txt"), col_names = FALSE)
  # lusmatrix.txt
  # write_delim(x = as_tibble(outMat, .name_repair = "minimal"), path = paste0(root, "lusmatrix.txt"), col_names = FALSE)
  # demand.in1
  # writeLines(outDemand, paste0(opts$path$model, "/demand.in1"))
  # main.1
  # writeLines(outMain, paste0(opts$path$model, "/main.1"))
  #
  #
  # cov_all.0.asc
  # region.fil.asc
  # writeRaster(x = region,
  #             filename = paste0(root, "region.fil.asc"),
  #             overwrite = TRUE,
  #             format = "ascii",
  #             datatype = "INT1U")
  # sc1gr[1-X].fil.asc

  return(scene)
}
