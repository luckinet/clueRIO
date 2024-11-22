#' Start a new project
#'
#' This function registers initial metadata of the current project into the
#' global options.
#' @param root [`character(1)`][character]\cr path to the root directory that contains
#'   or shall contain the working directory of this project.
#' @param name [`character(1)`][character]\cr the name for this project.
#' @param start [`integerish(1)`][integer]\cr the first year of the project.
#' @param end [`integerish(1)`][integer]\cr the last year of the project.
#' @param landuse [`character(.)`][character]\cr the allowed/valid landuse systems for
#'   this project.
#' @param demands [`character(.)`][character]\cr the allowed/valid demand types for
#'   this project.
#' @param resolution [`numeric(2)`][numeric]\cr vector of x and y resolution.
#' @param extent [\code{extent(1)}]\cr maximum extent of the region,
#'   raster::extent object.
#' @return Enters the provided information as 'lclu' into the options.
#' @importFrom checkmate testDirectory assertCharacter assertIntegerish
#' @export

startProject <- function(root = NULL, name = NULL, start = NULL, end = NULL,
                         landuse = NULL, demands = NULL, resolution = NULL,
                         extent = NULL){

  .Deprecated("clue_scene")

  assertCharacter(x = name, any.missing = FALSE, len = 1)
  assertIntegerish(x = start, any.missing = FALSE, len = 1)
  assertIntegerish(x = end, any.missing = FALSE, len = 1)
  assertCharacter(x = landuse, any.missing = FALSE, min.len = 1)
  assertCharacter(x = demands, any.missing = FALSE, min.len = 1)
  assertCharacter(x = resolution, any.missing = FALSE, null.ok = TRUE)
  assertCharacter(x = extent, any.missing = FALSE, null.ok = TRUE)

  # shitty windows workaround, because a directory may not have a trailing slash
  # for the function "file.exists()" used in assertDirectory()
  lastChar <- substr(x = root, start = nchar(root), stop = nchar(root))
  if (lastChar == "/") {
    root <- substr(root, start = 1, stop = nchar(root) - 1)
  }

  # test whether the required directories exist and create them if they don't exist
  if (!testDirectory(x = root, access = "rw")) {
    dir.create(file.path(root))
    message("I have created a new project directory.\n")
  }

  wd <- file.path(root, name)
  if (!testDirectory(x = wd, access = "rw")) {
    dir.create(wd, recursive = TRUE)
    dir.create(file.path(wd, "backup"))
    dir.create(file.path(wd, "input"))
    dir.create(file.path(wd, "output"))
    file.copy(from = system.file("CLUMondo.exe", package = "luckiCLU", mustWork = TRUE),
              to = paste0(wd, "/CLUMondo.exe"))
  } else {
    # if the paths already existed, clean it out
    current <- list.files(path = wd)
    toRemove <- current[-which(current %in% c("CLUMondo.exe", "backup", "input", "output"))]
    if(length(toRemove) != 0){
      file.remove(paste0(wd, "/", toRemove))
    }
  }


  if(length(list.files(wd, pattern = "cov_all.0")) == 0) {
    message("please create a gridded layer of the initial landuse with 'regGridded(name = \"landuse\", ...)'")
  }

  # test number of landuse systems (not more than 30)
  if (length(landuse) > 30) {
    return("there are too many commodities!")
  }

  newlLus <- newAllow <- newTypes <- newSuit <- newNeigh <- tibble(system = landuse)
  newlLus[,demands] <- 0
  newAllow[,landuse] <- 0
  newTypes[,landuse] <- NA_character_
  newSuit$alloc <- NA_character_
  newSuit$const <- NA_real_
  newNeigh$alloc <- NA_character_
  newNeigh$neighmat <- NA_character_
  newNeigh$const <- NA_real_
  newNeigh$weight <- 0


  opts <- list(path = list(root = root,
                           model = wd),
               meta = list(name = name,
                           time = paste0(strsplit(x = format(Sys.time(), format="%Y%m%d_%H%M%S"), split = "[ ]")[[1]], collapse = "_"),
                           first_year = start,
                           last_year = end,
                           stepwise = FALSE,
                           cells = NULL,
                           resolution = resolution,
                           extent = extent),
               attributes = list(gridded = tibble(driver = character(),
                                                  file = character(),
                                                  dynamic = logical()),
                                 landuse = tibble(system = landuse,
                                                  resistance = NA_real_,
                                                  preference = NA_real_),
                                 demand = tibble(type = demands,
                                                 match = NA_character_)),
               tables = list(main = tibble(var = c("ls_types_n", "regions_n", "drivers_max", "drivers_tot",
                                                   "demand_types_n", "mapRows_n", "mapCols_n", "map_res",
                                                   "map_xmin", "map_ymin", "ls_types", "resistance",
                                                   "demand_type", "iter_vars", "years", "drivers_dyn",
                                                   "out_type", "rs_reg", "ls_hist", "neigh", "loc_pref",
                                                   "dyn_lusmat", "out_write", "iter_param"),
                                           value = c(NA, 1, 1, 0, NA, NA,
                                                     NA, NA, NA, NA, NA, NA,
                                                     NA, paste(c(0, 0.5, 1), collapse = "\t"), paste(c(start, end), collapse = "\t"), NA, 3, 0,
                                                     paste(c(1, 5), collapse = "\t"), 0, 0, 0, 1, 0.05)),
                             alloc1 = newSuit,
                             alloc2 = newNeigh,
                             allow = newAllow,
                             lusconv = newlLus,
                             lusmatrix = newlLus,
                             allowTypes = newTypes),
               data = list(regions = NULL,
                           demand = NULL),
               files = list(),
               modifier = NULL)

  options(lclu = NULL)
  options(lclu = opts)
}
