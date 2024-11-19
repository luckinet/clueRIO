#' Set land-use types
#'
#' This function registers land-use types.
#' @param name [`character(1)`][character]\cr the name of the land-use type.
#' @param resistance [`numeric(1)`][numeric]\cr the relative conversion
#'   resistance to land-use change.
#' @param limit [`integerish(1)`][integer]\cr the number of time-steps a
#'   land-use type is allowed to remain in a location at most.
#' @param table [`data.frame(1)`][data.frame]\cr table of information on
#'   land-use types. Must contain the columns \code{name}, \code{resistance},
#'   \code{limit} and \code{target}.
#' @family functions to set new items
#' @importFrom dplyr bind_cols filter mutate if_else sym
#' @importFrom tibble add_row
#' @importFrom stringr str_trim
#' @importFrom checkmate assertCharacter assertNumeric assertIntegerish
#'   assertLogical assertDataFrame assertNames assertChoice
#' @export

regLanduse <- function(name = NULL, resistance = NULL, limit = NULL, table = NULL){

  opts <- getOption("lclu")

  assertCharacter(x = name, any.missing = FALSE, len = 1)
  assertNumeric(x = resistance, any.missing = FALSE, len = 1, lower = 0, upper = 1)
  assertIntegerish(x = limit, any.missing = FALSE, len = 1, lower = 0, null.ok = TRUE)
  assertDataFrame(x = table, any.missing = FALSE, null.ok = TRUE)

  if (!is.null(table)) {
    assertNames(x = colnames(table), must.include = c("name", "resistance", "limit", "target"))
    assertCharacter(x = table$name, any.missing = FALSE)
    assertNumeric(x = table$resistance, any.missing = FALSE)
    assertNumeric(x = table$limit, any.missing = FALSE)
    assertLogical(x = table$target, any.missing = FALSE)
  }

  # test that 'name' is a valid landuse system
  targetSystems <- opts$attributes$landuse$system
  assertChoice(x = name, choices = targetSystems)

  # set the limit in allow.txt
  allow <- opts$tables$allow
  if(is.null(limit)){
    temp <- 1
    label <- ""
  } else {
    temp <- -100-limit
    label <- paste0("limited, ", limit)
  }
  allow[name][which(allow$system == name),] <- temp
  opts$tables$allow <- allow

  type <- opts$tables$allowTypes
  type[name][which(type$system == name),] <- label
  opts$tables$allowTypes <- type

  # update resistance
  opts$attributes$landuse$resistance[opts$attributes$landuse$system == name] <- resistance


  options(lclu = opts)
  invisible(name)

}