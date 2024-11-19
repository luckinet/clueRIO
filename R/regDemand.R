#' Set the table of demand
#'
#' This function
#' @param table [`data.frame(1)`][data.frame]\cr
#' @param region [`character(1)`][character]\cr the column/variable in
#'   \code{table} that distinguishes unique regions of that object.
#' @family functions to register items
#' @importFrom checkmate assertDataFrame assertCharacter assertNames
#' @importFrom dplyr filter distinct rename arrange
#' @importFrom tidyr pivot_wider
#' @export

regDemand <- function(table = NULL, values = NULL, region = NULL){

  opts <- getOption("lclu")

  assertDataFrame(x = table)
  assertCharacter(x = values, len = 1)
  assertCharacter(x = region, any.missing = FALSE, len = 1)
  assertNames(x = names(table), must.include = c("year", "commodity", region, values))
  assertTRUE(x = dim(distinct(table[region]))[2] == 1)

  demand <- table %>%
    filter(year >= opts$meta$first_year & year <= opts$meta$last_year) %>%
    filter(commodity %in% opts$attributes$demand$type) %>%
    filter(!is.na(commodity)) %>%
    distinct(.keep_all = TRUE)

  if(dim(demand)[1] == 0) {
    stop("after filtering for target years and commodities, the tables doesn't contain any entries anymore.")
  }

  availComm <- opts$attributes$demand$type %in% unique(demand$commodity)

  demand <- demand %>%
    dplyr::select(c("year", "commodity", region, all_of(values))) %>%
    pivot_wider(names_from = commodity, values_from = values) %>%
    rename(region = region) %>%
    arrange(year) %>%
    dplyr::select(region, year, opts$attributes$demand$type[availComm])

  opts$data$demand <- demand

  options(lclu = opts)

}