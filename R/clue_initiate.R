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
#' @importFrom checkmate assertClass assertFileExists assertTRUE testDirectory
#' @export

clue_initiate <- function(scene, clue.exe = NULL){

  assertClass(x = scene, classes = "scene")
  assertCharacter(x = clue.exe, len = 1, any.missing = FALSE, null.ok = TRUE)

  assertTRUE(x = scene@validated)

  # model directory
  root <- file.path(scene@meta$path)

  # create executable ----
  if(is.null(clue.exe)){
    file.copy(from = system.file("CLUMondo.exe", package = "clueRIO", mustWork = TRUE),
              to = paste0(root, "CLUMondo.exe"))
  } else {
    assertFileExists(x = clue.exe, access = "r", extension = "exe")
    file.copy(from = clue.exe,
              to = paste0(root, "CLUMondo.exe"))
  }

  # save landsystems.tif as cov_all.0 ----
  # if(length(list.files(wd, pattern = "cov_all.0")) == 0) {
  #   message("please create a gridded layer of the initial landuse with 'regGridded(name = \"landuse\", ...)'")
  # }


  # insert code from makeModel.R here
  # insert code from testModel.R here



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
  # opts <- list(path = list(root = root,
  #                          model = wd),
  #              meta = list(name = name,
  #                          time = paste0(strsplit(x = format(Sys.time(), format="%Y%m%d_%H%M%S"), split = "[ ]")[[1]], collapse = "_"),
  #                          first_year = start,
  #                          last_year = end,
  #                          stepwise = FALSE,
  #                          cells = NULL,
  #                          resolution = resolution,
  #                          extent = extent),
  #              attributes = list(gridded = tibble(driver = character(),
  #                                                 file = character(),
  #                                                 dynamic = logical()),
  #                                landuse = tibble(system = landuse,
  #                                                 resistance = NA_real_,
  #                                                 preference = NA_real_),
  #                                demand = tibble(type = demands,
  #                                                match = NA_character_)),
  #              tables = list(main = tibble(var = c("ls_types_n", "regions_n", "drivers_max", "drivers_tot",
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
  # - cov_all.0
  # - region.fil
  # - sc1gr[1-X].fil


  return(scene)
}
