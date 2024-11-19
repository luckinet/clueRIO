#' Create a new CLUE scene
#'
#' @param root [`character(1)`][character]\cr path where the model files will be
#'   stored.
#' @param name [`character(1)`][character]\cr the name of the scene.
#' @param period [`integerish(2)`][integer]\cr start and end year of the scene.
#' @param description [`character(1)`][character]\cr the description of the
#'   scene.
#' @details
#' @return an empty CLUE scene that captures initial metadata of the CLUE model
#'   run, but doesn't contain any parameters yet.
#' @importFrom checkmate assertCharacter assertDirectoryExists assertIntegerish
#' @importFrom stringr str_replace_all
#' @importFrom utils packageVersion
#' @importFrom methods new
#' @export

clue_scene <- function(root, name, period, description = NULL){

  assertCharacter(x = name, len = 1, null.ok = TRUE)
  assertDirectoryExists(x = root, access = "rw")
  assertIntegerish(x = period, len = 2, any.missing = FALSE)
  assertCharacter(x = description, len = 1, null.ok = TRUE)

  if(is.null(description)) description <- NA_character_

  # set a name, in case none is provided
  if(is.null(name)){
    name <- paste0("clue_", paste0(sample(c(LETTERS, letters), 12, TRUE), collapse = ""))
  }

  pkg_version <- str_replace_all(string = as.character(packageVersion("clueRIO")), pattern = "[.]", replacement = "")
  r_version <- str_replace_all(string = paste0(version$major, version$minor), pattern = "[.]", replacement = "")
  version <- paste0(paste0(c(pkg_version, r_version, format(Sys.Date(), "%Y%m%d")), collapse = "."))

  # put together the initial scene
  out <- new(Class = "scene",
             meta = list(
               name = name,
               version = version,
               description = description),
             period = period,
             landtypes = list(),
             commodities = list(),
             landsystems = tibble(ID = NA_integer_, system = NA_character_),
             drivers = list(),
             validated = FALSE)

  # store the root and options in the R-options
  oldOptions <- options()
  on.exit(options(oldOptions))

  opts <- list(root = root,
               options = tibble(var = c("ls_types_n", "regions_n", "drivers_max", "drivers_tot",
                                        "demand_types_n", "mapRows_n", "mapCols_n", "map_res",
                                        "map_xmin", "map_ymin", "ls_types", "resistance",
                                        "demand_type", "iter_vars", "years", "drivers_dyn",
                                        "out_type", "rs_reg", "ls_hist", "neigh", "loc_pref",
                                        "dyn_lusmat", "out_write", "iter_param"),
                                value = c(NA, 1, 1, 0, NA, NA,
                                          NA, NA, NA, NA, NA, NA,
                                          NA, paste(c(0, 0.5, 1), collapse = "\t"), paste(c(period[1], period[2]), collapse = "\t"), NA, 3, 0,
                                          paste(c(1, 5), collapse = "\t"), 0, 0, 0, 1, 0.05)))

  options(clue = opts)

  return(out)
}
