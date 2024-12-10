#' Set land types and their attributes
#'
#' @param scene [`scene(1)`][scene]\cr the CLUE scene to which to add a land
#'   type.
#' @param name [`character(1)`][character]\cr name of the land type.
#' @param resistance [`numeric(1)`][numeric]\cr the relative conversion
#'   resistance to land change.
#' @param suitability [`data.frame`][data.frame]\cr \emph{obligatory} table of
#'   the result of regression analysis. Has at least two rows. Must have columns
#'   \itemize{
#'    \item \code{driver}: from line 2 onward the name of the driver (must
#'          correspond to names of gridded items of \code{type = "driver"} as
#'          defined in \code{\link{clue_gridded}}) and
#'    \item \code{beta}: the respective regression coefficient
#'          (beta_\{1, 2, ...\}).}
#'   The first row must have the driver \code{"const"} with the respective
#'   \emph{beta_0} value.
#' @param production [`data.frame`][data.frame]\cr optional table of goods and
#'   services produced by this land type (for the demand to produce, see
#'   \code{\link{clue_goods}}). Each row contains one good. Must have 3 columns
#'   \itemize{
#'    \item \code{name}: the name of the good or service,
#'    \item \code{amount}: how much each cell in the map produces and
#'    \item \code{priority}: an integer value that indicates the priority with
#'          which the good or service is produced by the focal land type.
#'          If a good is produced by more than one land type, the land type with
#'          a smaller number here is produced with a higher priority.}
#' @param conversion [`data.frame`][data.frame]\cr optional table of land types
#'   into which this land type can be converted. Each row contains one type into
#'   which it can be converted. Must have a columns \itemize{
#'    \item \code{to}: name of the land type into which to convert and
#'    \item \code{label}: name of the conversion process,
#'   } and optionally, for the details of conversion one of \itemize{
#'    \item \code{min}: number of time-steps that need to pass until the
#'          conversion can take place,
#'    \item \code{max}: number of time-steps within which the conversion can
#'          take place,
#'    \item \code{auto}: number of time-steps after which a conversion must take
#'          place (from local dynamics and not to fulfill demand of a good or
#'          service) or
#'    \item \code{mask}: name of a gridded item (with values 0, 1 and NA/-9999)
#'          that indicates with value 1 where conversion can take place and with
#'          0, where they cannot take place.}
#' @param neighborhood [`data.frame`][data.frame]\cr optional table of
#'   neighborhood parameters. Has at least four rows. Must have columns
#'   \itemize{
#'    \item \code{type}: from line 4 onward the name of the land type that
#'          influences the suitability of the focal land type if it's in the
#'          neighborhood (must correspond to names of other land types defined
#'          with this function) and
#'    \item \code{beta}: the respective regression coefficient
#'          (beta_\{1, 2, ...\})}
#'   The first row has the number of a kernel that must have been defined with
#'   \code{clue_gridded} and the second row the weight assigned to the
#'   neighborhood function, and the third row must have the type \code{"const"}
#'   with the respective \emph{beta_0} value.
#' @param preference [`data.frame`][data.frame]\cr optional table of suitability
#'   preferences. One row only. Must have columns \code{layer} and
#'   \code{weight}. Layer names must correspond to names of gridded items of
#'   \code{type = "preference"} as defined in \code{\link{clue_gridded}}.
#' @section Suitability:
#'
#'   mention specifics about suitabilities
#'
#' @section Production:
#'
#'   mention specifics about production
#'
#' @section Conversion:
#'
#'   Conversions are implicitly always set from a focal class to itself. By
#'   default, this conversion is allowed indefinitely and one does not have to
#'   set it. However, if one wants to limit the persistence of a land type to,
#'   e.g., 5 years in a row, one needs to specify
#'
#'   ```
#'   clue_landtype(...,
#'                 name = "it",
#'                 conversion = tibble(to = "it",
#'                                     label = "limit_self",
#'                                     max = 5))
#'    ```
#'
#'
#' @section Neighborhood:
#'
#'   mention specifics about neighborhood
#'
#' @section Preference:
#'
#'   mention specifics about preferences
#'
#' @return
#' @examples
#' @importFrom checkmate assertClass assertCharacter assertDataFrame assertList
#'   assertNames assertChoice assertNumber
#' @importFrom tibble tibble as_tibble add_column add_row
#' @importFrom dplyr bind_cols bind_rows mutate everything case_when
#' @importFrom tidyr pivot_wider pivot_longer nest
#' @importFrom rlang sym
#' @export

clue_landtype <- function(scene, name, resistance, suitability,
                          production = NULL, conversion = NULL,
                          neighborhood = NULL, preference = NULL){

  assertClass(x = scene, classes = "scene")
  assertCharacter(x = name, len = 1, any.missing = FALSE)
  assertDataFrame(x = suitability, min.rows = 1, ncols = 2, all.missing = FALSE)
  assertDataFrame(x = production, all.missing = FALSE, null.ok = TRUE)
  assertDataFrame(x = conversion, min.cols = 2, all.missing = FALSE, null.ok = TRUE)
  assertDataFrame(x = neighborhood, all.missing = FALSE, null.ok = TRUE)
  assertDataFrame(x = preference, nrows = 1, all.missing = FALSE, null.ok = TRUE)

  opts <- getOption("clue")

  # make sure that gridded layers are available
  if(length(scene@grids) == 0){
    stop("please register the required gridded layers first; clue_gridded(...)")
  }

  # handle suitability ----
  #
  assertNames(x = names(suitability), must.include = c("driver", "beta"))
  assertNames(x = suitability$driver, must.include = "const")
  suitability <- suitability |>
    pivot_wider(names_from = driver, values_from = beta)

  # handle production ----
  #
  if(!is.null(production)){
    assertNames(x = names(production), must.include = c("name", "amount", "priority"))
  } else {
    production <- tibble(name = NA_character_, amount = NA_real_, priority = NA_real_)
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
        conv == "min" ~ as.character(100 + as.integer(values)),
        conv == "max" ~ as.character(-100 - as.integer(values)),
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
    assertNames(x = names(neighborhood), subset.of = c("type", "beta"))
    assertNames(x = neighborhood$type, must.include = c("kernel", "weight", "const"))
    neighborhood <- neighborhood |>
      pivot_wider(names_from = type, values_from = beta)

    # activate in options
    opts$value[opts$var == "neigh"] <- as.character(1)
    options(clue = opts)
  } else {
    neighborhood <- tibble(kernel = NA_real_, weight = NA_real_, const = NA_real_)
  }

  # handle preference ----
  #
  if(!is.null(preference)){
    assertNames(x = names(preference), must.include = c("layer", "weight"))
    assertNumber(x = preference$weight, lower = 0, upper = 1, finite = TRUE)

    # activate in options
    opts$value[opts$var == "loc_pref"] <- as.character(1)
    options(clue = opts)
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
