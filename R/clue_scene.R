#' Create a new CLUE scene
#'
#' @param root [`character(1)`][character]\cr path where the model files will be
#'   stored.
#' @param name [`character(1)`][character]\cr the name of the scene.
#' @param period [`integerish(2)`][integer]\cr start and end year of the scene.
#' @param regions description
#' @param description [`character(1)`][character]\cr the description of the
#'   scene.
#' @details
#' @return object of class \code{\link{scene}} that contains initial meta data.
#' @examples
#' library(sf)
#' region <- st_read(
#'   paste0(system.file("test_data", package = "clueRIO"),
#'          "/test_region.gpkg"))
#'
#' minimal <- clue_scene(
#'     root = paste0(tempdir(), "/ls2d1"),
#'     name = "ls2d1",
#'     period = c(2000, 2024),
#'     regions = region,
#'     description = "this is a minimal showcase where forest is converted to
#'                    cropland to produce crops.")
#' @importFrom checkmate assertCharacter assertDirectoryExists assertIntegerish
#' @importFrom stringr str_replace_all
#' @importFrom sf write_sf
#' @importFrom tibble tibble
#' @importFrom utils packageVersion
#' @importFrom rlang new_environment
#' @importFrom methods new
#' @export

clue_scene <- function(root, name, period, regions, description = NULL){

  assertCharacter(x = name, len = 1, null.ok = TRUE)
  assertIntegerish(x = period, len = 2, any.missing = FALSE)
  assertCharacter(x = description, len = 1, null.ok = TRUE)

  if(substr(x = root, start = nchar(root), stop = nchar(root)) != "/"){
    root <- paste0(root, "/")
  }

  if(is.null(description)) description <- NA_character_

  # set a name, in case none is provided
  if(is.null(name)){
    name <- paste0("clue_", paste0(sample(c(LETTERS, letters), 12, TRUE), collapse = ""))
  }

  # create the directory ./root/name if it doesn't exist yet
  if(!testDirectoryExists(x = root)){
    message("creating model directory ", root, ".")
    dir.create(path = root)
  }
  if(!testDirectoryExists(x = file.path(root, "input"))){
    dir.create(path = file.path(root, "input"))
  }
  if(!testDirectoryExists(x = file.path(root, "output"))){
    dir.create(file.path(root, "output"))
  }

  pkg_version <- str_replace_all(string = as.character(packageVersion("clueRIO")), pattern = "[.]", replacement = "")
  r_version <- str_replace_all(string = paste0(version$major, version$minor), pattern = "[.]", replacement = "")
  version <- paste0(paste0(c(pkg_version, r_version, format(Sys.Date(), "%Y%m%d")), collapse = "."))

  # handle model regions
  isSF <- testClass(x = regions, classes = "sf")
  if(!isSF){
    assertFileExists(x = regions, access = "rw")
    file.copy(from = regions, to = paste0(root, "input/regions.gpkg"), overwrite = TRUE)
  } else {
    write_sf(obj = regions, dsn = paste0(root, "input/regions.gpkg"), quiet = TRUE)
  }

  opts <- tibble(var = c("ls_types_n", "regions_n", "drivers_max", "drivers_tot",
                         "demand_types_n", "mapRows_n", "mapCols_n", "map_res",
                         "map_xmin", "map_ymin", "ls_types", "resistance",
                         "demand_type", "iter_vars", "years", "drivers_dyn",
                         "out_type", "rs_reg", "ls_hist", "neigh", "loc_pref",
                         "dyn_lusmat", "out_write", "iter_param"),
                 value = c(NA, 1, 1, 0, NA, NA,
                           NA, NA, NA, NA, NA, NA,
                           NA, paste(c(0, 0.5, 1), collapse = "\t"), paste(c(period[1], period[2]), collapse = "\t"), NA, 3, 0,
                           paste(c(1, 5), collapse = "\t"), 0, 0, 0, 1, 0.05))

  # store the root and options in the R-options
  oldOptions <- options()
  on.exit(options(oldOptions))

  options(clue = opts)

  # put together the initial scene
  out <- new(Class = "scene",
             meta = list(
               name = name,
               version = version,
               path = root,
               description = description),
             period = period,
             grids = list(),
             landtypes = list(),
             goods = list())

  return(out)
}
