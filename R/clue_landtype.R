#' Set land types and their attributes
#'
#' @param scene description
#' @param name [`character(1)`][character]\cr name of the land type.
#' @param resistance [`numeric(1)`][numeric]\cr the relative conversion
#'   resistance to land change.
#' @param suitability [`data.frame(2)`][data.frame]\cr obligatory table of the
#'   result of regression analysis. At least two rows. Includes the constant
#'   (\emph{beta_0}) and as many drivers and their beta coefficient as there are
#'   in the regression formula (\emph{beta_1}, \emph{beta_2}, ...). Must have
#'   the column \code{const} and at least one other column with the name of the
#'   respective suitability driver. Driver names must correspond to names of
#'   gridded items of \code{type = "driver"} as defined in
#'   \code{\link{clue_gridded}}.
#' @param production [`data.frame(3)`][data.frame]\cr optional table of goods
#'   and services produced by this land type. Each row contains one good. Must
#'   have column \code{goods} for its name, \code{amount} of production and
#'   \code{priority}.
#' @param conversion [`data.frame(.)`][data.frame]\cr optional table of land
#'   types into which this land type can be converted. Each row contains one
#'   type into which it can be converted. Must have a column \code{to} and
#'   \code{label} and for the details of conversion, optionally one of
#'   \code{min}, \code{max},\code{auto} or \code{mask}.
#' @param neighborhood [`data.frame(.)`][data.frame]\cr work in progress
#' @param preference [`data.frame(2)`][data.frame]\cr optional table of
#'   suitability preferences. One row only. Must have columns \code{layer} and
#'   \code{weight}. Layer names must correspond to names of gridded items of
#'   \code{type = "preference"} as defined in \code{\link{clue_gridded}}.
#' @details
#' @return
#' @examples
#' @importFrom checkmate assertClass assertCharacter assertDataFrame assertList
#'   assertNames assertChoice assertNumber
#' @importFrom tibble tibble as_tibble add_column add_row
#' @importFrom dplyr bind_cols bind_rows mutate everything case_when
#' @importFrom tidyr pivot_wider pivot_longer nest
#' @importFrom rlang sym
#' @export

clue_landtype <- function(scene, name, resistance = NULL, suitability = NULL,
                          production = NULL, conversion = NULL,
                          neighborhood = NULL, preference = NULL){

  assertClass(x = scene, classes = "scene")
  assertCharacter(x = name, len = 1, any.missing = FALSE)
  assertDataFrame(x = suitability, min.rows = 1, ncols = 2, all.missing = FALSE)
  assertDataFrame(x = production, all.missing = FALSE, null.ok = TRUE)
  assertDataFrame(x = conversion, min.cols = 2, all.missing = FALSE, null.ok = TRUE)
  assertDataFrame(x = neighborhood, all.missing = FALSE, null.ok = TRUE)
  assertDataFrame(x = preference, nrows = 1, all.missing = FALSE, null.ok = TRUE)

  # make sure that gridded layers are available
  if(length(scene@grids) == 0){
    stop("please register the required gridded layers first; clue_gridded(...)")
  }

  # handle suitability ----
  #
  assertNames(x = names(suitability), must.include = c("driver", "beta"))
  suitability <- suitability |>
    pivot_wider(names_from = driver, values_from = beta)

  # handle production ----
  #
  if(!is.null(production)){
    assertNames(x = names(production), must.include = c("goods", "amount", "priority"))
  } else {
    production <- tibble(goods = NA_character_, amount = NA_real_, priority = NA_real_)
  }

  # handle conversion ----
  #
  if(!is.null(conversion)){
    assertNames(x = names(conversion), must.include = c("to", "label"), subset.of = c("to", "label", "min", "max", "auto", "mask"))
    assertIntegerish(x = rowSums(!is.na(conversion))-2, upper = 1, .var.name = "n_parameters(conversion)")
    if(!any(c("min", "max", "auto", "mask") %in% names(conversion))){
      conversion <- conversion |>
        mutate(allow = 1)
    }
    conversion <- conversion |>
      mutate(across(everything(), as.character)) |>
      pivot_longer(cols = c(-to, -label), names_to = "conv", values_to = "values") |>
      filter(!is.na(values)) |>
      mutate(value = suppressWarnings(case_when(
        conv == "min" ~ as.character(-100 - as.integer(values)),
        conv == "max" ~ as.character(100 + as.integer(values)),
        conv == "auto" ~ as.character(1000 + as.integer(values)),
        conv == "mask" ~ values,
        .default = "1"
      ))) |>
      select(-conv, -values)

    if(!any(conversion$to %in% name)){
      conversion <- conversion |>
        add_row(to = name, label = "self", value = "1")
    }
  } else {
    conversion <- tibble(to = name, label = "self", value = "1")
  }
  conversion <- conversion |>
    pivot_longer(cols = -to) |>
    pivot_wider(names_from = to, values_from = value) |>
    nest()

  # handle neighborhood ----
  #
  if(!is.null(neighborhood)){
    stop("'neighborhood' is currently work in progress")
    # assertNames(x = names(neighborhood), subset.of = c("const", "weight", "kernel"))
  } else {
    neighborhood <- tibble(const = NA_real_, weight = NA_real_, kernel = NA)
  }

  # handle preference ----
  #
  if(!is.null(preference)){
    assertNames(x = names(preference), must.include = c("layer", "weight"))
    assertNumber(x = preference$weight, lower = 0, upper = 1, finite = TRUE)
  } else {
    preference <- tibble(layer = NA_character_, weight = NA_real_)
  }


  # store parameters ----
  #
  temp <- list(resistance = resistance,
               production = production,
               suitability = suitability,
               conversion = conversion,
               neighborhood = neighborhood,
               preference = preference)

  scene@landtypes[[name]] <- temp

  # save items ----



  return(scene)
}
