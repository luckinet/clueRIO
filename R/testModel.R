#' Test the model specification
#'
#' @importFrom dplyr select filter mutate across left_join filter_if rowwise
#' @importFrom stringr str_split
#' @importFrom tidyselect all_of everything where
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
#' @export

testModel <- function(){

  opts <- getOption("lclu")

  # get objects involved in the tests here
  demand <- opts$data$demand
  rules <- opts$tables$allow
  production <- opts$tables$lusmatrix
  attrib <- opts$attributes$landuse
  main <- opts$tables$main
  suit <- opts$tables$alloc1
  neigh <- opts$tables$alloc2

  # and derive some more
  actualDemand <- demand |>
    dplyr::select(-region, -year) |>
    names()
  initialDemand <- demand |>
    filter(year == opts$meta$first_year)
  conv <- as.numeric(str_split(main[["value"]][14], "\t")[[1]])

  diff2 <- function(x){
    c(0, diff(x))
  }

  change <- demand |>
    dplyr::select(all_of(actualDemand)) |>
    mutate(across(.cols = everything(),
                  .fns = list(diff2),
                  .names = "{col}")) |>
    mutate(across(.cols = everything(),
                  .fns = list(cumsum),
                  .names = "{col}"))


  # test that the combined demand is not larger than the amount of pixels
  maxDemand <- demand |>
    rowwise() |>
    mutate(sum = sum(across(all_of(actualDemand))))
  if(any(maxDemand$sum > opts$meta$cells)){
    stop("the overall demand for '", paste(demand$year[which(maxDemand$sum > opts$meta$cells)], collapse = ", "),"' is larger than the number of cells (", opts$meta$cells, ").")
  }

  # test that all project years are in the demand table
  aimYears <- seq(from = opts$meta$first_year, to = opts$meta$last_year, by = 1)
  actualYears <- unique(opts$data$demand$year)
  if(!all(aimYears %in% actualYears)){
    stop(paste0("not all project years are in the demand table (", paste0(aimYears[which(!aimYears %in% actualYears)], collapse = ", "), ")\n   -> please either provide the missing demand or adapt the project years."))
  }

  # test that each landuse type has a conversion rule specified at least to itself
  nonValid <- rules |>
    filter(across(where(is.numeric), ~ . == 0))
  if(dim(nonValid)[1] != 0){
    stop("the landuse type '", paste(nonValid$system, collapse = ", "), "' does not have a valid set of conversion rules.")
  }

  # test that each landuse type has a suitability specification
  nonValid <- suit |>
    filter(is.na(const))
  if(dim(nonValid)[1] != 0){
    stop("the landuse type '", paste(nonValid$system, collapse = ", "), "' does not have a valid suitability specification.")
  }

  # test that each landuse type has a neighbourhood specification
  if(opts$tables$main[which(opts$tables$main$var == "neigh"),]$value == 1){
    nonValid <- neigh |>
      filter(is.na(const))
    if(dim(nonValid)[1] != 0){
      stop("the landuse type '", paste(nonValid$system, collapse = ", "), "' does not have a valid neighbourhood specification.")
    }
  }

  # test that each landuse type has location specific preferences
  if(any(!is.na(attrib$preference))){
    nonValid <- attrib |>
      filter(is.na(preference))
    if(dim(nonValid)[1] != 0){
      stop("the landuse type '", paste(nonValid$system, collapse = ", "), "' does not have valid location specific preferences.")
    }

    # # and whether modifier is set
    # if(any(attrib$preference != 0)){
    #
    # }
  }

  # test that main has been properly specified


  # test for each actual demand a couple of things
  for(i in actualDemand){

    # determine the landuse types that produce this demand
    prodTypes <- production |>
      filter(!!sym(i) != 0)

    # test that the demand has a production specification
    if(dim(prodTypes)[1] == 0){
      stop("'", i, "' is not produced by any landuse type.")
    }

    # get first years demand
    firstDemand <- initialDemand[[i]]

    # determine the total amount of the already produced demand
    prodDemand <- left_join(prodTypes, attrib, by = "system")
    prodDemand <- sum(prodDemand[[i]] * prodDemand[["cells"]])

    # determine the landuse types this landuse type can be converted to and
    # from, and the number of cells each of the allowed landuse types has in the
    # initial landuse map (cov_all.0)
    allowedTo <- rules |>
      filter(system %in% prodTypes$system) |>
      pivot_longer(-system, names_to = "to", values_to = "allowed") |>
      filter(allowed != 0) |>
      left_join(attrib, by = c("to" = "system"))
    allowedFrom <- rules |>
      dplyr::select("system", prodTypes$system) |>
      pivot_longer(-system, names_to = "to", values_to = "allowed") |>
      filter(allowed != 0) |>
      left_join(attrib, by = "system")

    if(dim(allowedFrom)[1] == 1){
      reason1 <- paste0("\n  --> no landuse type can be converted to '", paste(allowedFrom$to, collapse = ", "), "', producing '", i, "'.")
    } else {
      reason1 <- NULL
    }
    if(dim(allowedTo)[1] == 1){
      reason2 <- paste0("\n  --> '", paste(allowedTo$system, collapse = ", "), "', producing '", i, "', can't be converted to another landuse type.")
    } else {
      reason2 <- NULL
    }

    # determine the number of cells that are not already in use for demand
    # production, but which are allowed to produce more (i.e., which the target
    # landuse are allowed to be converted to or from due to the conversion
    # rules)
    remainingTo <- sum(allowedTo$cells) - prodDemand
    remainingFrom <- sum(allowedFrom$cells) - prodDemand

    # go through each demand change and check whether it's higher or lower than the remaining pixels
    for(j in seq_along(change[[i]])[-1]){

      thisChange <- change[[j,i]]

      if(thisChange > 0){
        if(thisChange > remainingFrom){
          stop("For ", demand$year[j], " CLUMondo will try to allocate more '", i, "' (", thisChange, ") than allowed (", remainingFrom, ").", reason1)
        }
      } else if(thisChange < 0){
        if(abs(thisChange) > remainingTo){
          stop("For ", demand$year[j], " CLUMondo will try to allocate less '", i, "' (", thisChange, ") than allowed (", remainingTo, ").", reason2)
        }
      }

    }

    # test that demand of the first year is actually the same as pixels in the
    # intial landuse multiplied with production amount.
    if(firstDemand != prodDemand){
      warning("The target demand (", firstDemand, ") and the produced demand (", prodDemand, ") of '", i, "' are not equal.")
    }

    # test that first and second convergence criteria are below the rate of
    # change of demand.
    testDemand <- demand |>
      dplyr::select(all_of(i))
    testDemand <- tibble(first = testDemand[[1]][-dim(testDemand)[1]], second = testDemand[-1,][[1]]) |>
      mutate(dev = abs((first/second)*100 - 100))

    if(any(testDemand$dev < conv[2])){
      warning("The first convergence criterion (", conv[2], ") is larger than the smallest amount of change in demand of '", i, "' (", round(min(testDemand$dev, na.rm = TRUE), 3), ").")
    }
    if(any(testDemand$dev < conv[3])){
      warning("The second convergence criterion (", conv[3], ") is larger than the smallest amount of change in demand of '", i, "' (", round(min(testDemand$dev, na.rm = TRUE), 3), ").")
    }

  }


  # # ensure that 'theRegion', which is used as mask, has the project crs
  # theRegion <- st_transform(x = theRegion, crs = crs(init$crs))
  #
  # # subset spatially and set nodatavalue
  # cov_all <- mask(cov_all, theRegion)
}
