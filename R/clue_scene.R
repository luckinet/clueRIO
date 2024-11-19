#' Create a new CLUE scene
#'
#' @param name [`character(1)`][character]\cr the name of the scene.
#' @param name [`character(1)`][character]\cr path where the model files will be
#'   stored.
#' @param period [`integerish(2)`][integer]\cr start and end year of the scene.
#' @param resolution [`numeric(2)`][numeric]\cr vector of x and y resolution.
#' @param description [`character(1)`][character]\cr the description of the
#'   scene.
#' @details
#' @return an empty CLUE scene that captures initial metadata of the CLUE model
#'   run, but doesn't contain any parameters yet.
#' @importFrom checkmate assertCharacter assertDirectoryExists assertIntegerish
#' @importFrom stringr str_replace_all
#' @export

clue_scene <- function(name, period, root, description = NULL){

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

  # store the root path in the options
  oldOptions <- options()
  on.exit(options(oldOptions))

  options(clue_path = root)

  return(out)
}
