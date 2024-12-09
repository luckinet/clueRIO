#' Initiate a CLUE scene
#'
#' This creates the initial model files for a CLUE scene while ensuring in
#' parallel that everything is set up properly.
#' @param scene [`scene(1)`][scene]\cr the CLUE scene for which to prepare the
#'   model directory.
#' @param module description
#' @details
#' @examples
#' @return no return value, called for the side-effect of creating directories
#'   and initial files according to the model scene.
#' @importFrom checkmate assertClass assertFileExists assertTRUE assertChoice
#' @importFrom purrr map_dfr map
#' @importFrom dplyr bind_cols bind_rows filter distinct select across all_of
#'   if_else cur_column rowwise left_join pull arrange any_of row_number rename
#' @importFrom tidyr pivot_wider unite unnest complete everything
#' @importFrom readr write_delim
#' @importFrom terra as.matrix rast writeRaster values<-
#' @export

clue_initiate <- function(scene, module = NULL){

  # library(dplyr); library(checkmate); library(tibble); library(purrr); library(readr); library(tidyr); library(stringr); opts_old <- readRDS(paste0(getwd(), "/_misc/old_functions/old_datastructure.rds"))

  assertClass(x = scene, classes = "scene")
  # assertChoice(x = module, choices = c(), null.ok = TRUE)

  # model directory
  root <- scene@meta$path
  opts <- getOption("clue")

  # .printOut <- function(x) {
  #   emptySpaces <- map(.x = seq_along(x), .f = \(ix) nchar(names(x)[ix])) |>
  #     unlist(use.names = FALSE) |>
  #     max(na.rm = TRUE)
  #
  #   for (i in seq_along(x)) {
  #     thisLen <- nchar(names(x)[i])
  #     temp <- paste0(yellow(paste0(
  #       names(x)[i], paste0(rep(" ", emptySpaces - thisLen), collapse = ""), ": "
  #     )), x[i], "\n")
  #     print(temp)
  #   }
  # }
  # list("directory" = "ok") |>
  #   .printOut()


  # local variables ----
  #
  .landtypes <- names(scene@landtypes)
  .goods <- names(scene@goods)
  .gridded <- names(scene@grids)

  ## build table of land type parameters ----
  tbl_landtypes <- map_dfr(.x = seq_along(scene@landtypes),
                           .f = function(ix){
                             bind_cols(type = names(scene@landtypes)[ix], bind_rows(scene@landtypes[[ix]]))
                           })

  ## build table of goods parameters ----
  tbl_goods <- map_dfr(.x = seq_along(scene@goods),
                       .f = function(ix){
                         bind_cols(goods = names(scene@goods)[ix], bind_rows(scene@goods[[ix]]))
                       })

  ## build table of gridded items ----
  tbl_gridded <- map_dfr(.x = seq_along(scene@grids),
                         .f = function(ix){
                           bind_cols(driver = names(scene@grids)[ix], bind_rows(scene@grids[[ix]]))
                         })

  ## build table of demand ----
  tbl_demand <- tbl_goods |>
    select(goods, demand) |>
    unnest(cols = demand) |>
    unnest(cols = data) |>
    filter(!is.na(goods)) |>
    distinct(.keep_all = TRUE) |>
    pivot_wider(names_from = goods, values_from = amount)


  ## extract the initial demand ----
  initialDemand <- tbl_demand |>
    filter(year == scene@period[1])


  # build output files ----
  #
  ## demand.in1 ----
  .demand <- tbl_demand |>
    filter(year >= scene@period[1] & year <= scene@period[2]) |>
    select(-region, -year) |>
    unite(col = "string", sep = "\t") |>
    unlist()

  paste0(length(.demand), "\n", paste0(.demand, collapse = "\n")) |>
    writeLines(paste0(root, "demand.in1"))


  ## alloc1.reg ----
  .alloc1 <- tbl_landtypes |>
    select(type, suitability) |>
    unnest(cols = suitability) |>
    mutate(across(any_of(.gridded), ~ if_else(!is.na(.x), paste(.x, which(.gridded %in% cur_column()) - 1), NA_character_)),
           ID = row_number() - 1) |>
    unite(col = drivers, any_of(.gridded), sep = "\n\t", na.rm = TRUE, remove = FALSE) |>
    rowwise() |>
    mutate(nr_drivers = sum(!is.na(across(any_of(.gridded))))) |>
    mutate(out = paste0(ID, "\n\t", # Number code for land type
                        const, "\n", # Constant of regression equation for land type (ß0)
                        nr_drivers, "\n\t", # Number of explanatory factors (sc1gr#.fil files) in the regression equation for that land type.
                        drivers, "\n"))  # On each line the beta coefficients (ß1, ß2, etc.) for the explanatory factor and the number code of the explanatory factor.

  paste(.alloc1$out, collapse = "\n") |>
    writeLines(paste0(root, "alloc1.reg"))


  ## allow.txt ----
  .allow <- tbl_landtypes |>
    select(type, conversion) |>
    unnest(cols = conversion) |>
    unnest(cols = data) |>
    filter(name == "value") |>
    select(all_of(.landtypes)) |>
    rowwise() |>
    mutate(across(everything(), ~if_else(is.na(.x),
                                         "0",
                                         if_else(is.na(suppressWarnings(as.integer(.x))),
                                                 paste0(tbl_gridded$id[which(.gridded %in% .x)], collapse = ""),
                                                 .x,
                                                 ptype = "character"))))

  write_delim(x = .allow, col_names = FALSE,
              file = paste0(root, "allow.txt"))


  # if active
  if(opts$value[opts$var == "neigh"] == "1"){

    ## alloc2.reg ----
    .alloc2 <- tbl_landtypes |>
      select(type, neighborhood) |>
      unnest(cols = neighborhood) |>
      mutate(across(any_of(.landtypes), ~ if_else(is.na(.x), 0, .x)),
             const = if_else(is.na(const), 0, const),
             ID = row_number() - 1) |>
      mutate(across(any_of(.landtypes), ~ paste(.x, which(.landtypes %in% cur_column()) - 1))) |>
      unite(col = factors, any_of(.landtypes), sep = "\n", na.rm = TRUE, remove = FALSE) |>
      mutate(nr_drivers = sum(!is.na(across(any_of(.gridded))))) |>
      mutate(out = paste0(ID, "\n", # Number code for land type
                          const, "\n", # Constant of neighborhood regression equation for land type (ß0)
                          nr_drivers, "\n", # Number of explanatory factors (land types) in the equation for that land type (for all these the weight - set in neighmat.txt - should be > 0 otherwise the neighborhood is not calculated)
                          factors, "\n")) # On each line the coefficients for the explanatory factors and the number code of the explanatory factor. In using the neighborhood function, the explanatory factors are the land use types and therefore the number codes are the number codes for the land use types.

    paste(.alloc2$out, collapse = "\n") |>
      writeLines(paste0(root, "alloc2.reg"))


    ## neighmat.txt ----
    .kernels <- tbl_gridded |>
      filter(type == "kernel")
    .kernels <- map(.x = 1:dim(.kernels)[1],
                    .f = function(ix){
                      temp <- filter(.kernels, id == ix)
                      as.matrix(rast(temp$file), wide = TRUE)
                    })
    outKernels <- map(.x = .kernels,
                      .f = \(x) map(1:dim(x)[1],
                                    function(ix){
                                      paste0(x[ix, ], collapse = " ")
                                    }) |>
                        paste0(collapse = "\n")) |>
      unlist()

    .neighmat <- tbl_landtypes |>
      select(type, neighborhood) |>
      unnest(cols = neighborhood) |>
      mutate(weight = if_else(is.na(weight), 0, weight)) |>
      mutate(nr_types = sum(!is.na(across(any_of(.landtypes))))) |>
      mutate(across(any_of(.landtypes), ~ which(.landtypes %in% cur_column()))) |>
      unite(col = types, any_of(.landtypes), sep = " ", na.rm = TRUE, remove = FALSE) |>
      rowwise() |>
      mutate(out = if_else(!is.na(kernel),
                           paste0(nr_types, " ", # number of land types and ...
                                  types, "\n\n", # the codes for the included types
                                  (dim(.kernels[[kernel]])[1] - 1) / 2, "\n", # radius of the kernel
                                  outKernels[kernel]), NA_character_)) # the kernel

    paste0(paste0(.neighmat$weight, collapse = " "), "\n\n", paste0(na.omit(.neighmat$out), collapse = "\n\n"), "\n") |>
      writeLines(paste0(root, "neighmat.txt"))
  }


  ## lusconv.txt ----
  .production <- tbl_landtypes |>
    select(type, production) |>
    unnest(cols = production) |> # also test here when there are more than one good per type
    complete(type, name,
             fill = list(priority = 0,
                         amount = 0)) |>
    arrange(match(type, .landtypes)) |>
    filter(!is.na(name))

  .exclusion <- tbl_goods |>
    select(goods, excluded) |>
    unnest(cols = excluded) |>
    unnest(cols = data) |>
    mutate(exclude = TRUE)

  .lusconv <- .production |>
    select(-amount) |>
    left_join(.exclusion, by = "type") |>
    mutate(priority = if_else(!is.na(exclude), -1, priority),
           exclude = NULL) |>
    pivot_wider(names_from = name, values_from = priority) |>
    select(all_of(.goods))

  write_delim(x = .lusconv, col_names = FALSE,
              file = paste0(root, "lusconv.txt"))


  ## lusmatrix.txt ----
  .lusmatrix <- .production |>
    select(-priority) |>
    pivot_wider(names_from = name, values_from = amount) |>
    select(all_of(.goods))

  write_delim(x = .lusmatrix, col_names = FALSE,
              file = paste0(root, "lusmatrix.txt"))

  # if active
  if(opts$value[opts$var == "loc_pref"] != "0"){
    ## locspec#.fil ----
    .prefs <- tbl_gridded |>
      filter(type == "preference") |>
      select(-type, -id) |>
      rename(layer = driver)
    .mainRast <- rast(tbl_gridded$file[tbl_gridded$type == "initial"])
    values(.mainRast) <- 0

    .locspec <- tbl_landtypes |>
      select(type, preference) |>
      unnest(cols = preference) |>
      left_join(.prefs, by = "layer") |>
      mutate(ID = row_number() - 1,
             weight = if_else(is.na(weight), 0, weight),
             file_out = if_else(is.na(file), paste0(root, "locspec", ID, ".fil.asc"), file)) |>
      rowwise() |>
      mutate(file_out = str_replace(file_out, pattern = paste0("_", layer, "_"), replacement = as.character(ID)))

    for(i in seq_along(.locspec$file)){
      thisLocSpec <- .locspec[i, ]

      if(is.na(thisLocSpec$layer)){
        writeRaster(x = .mainRast,
                    filename = thisLocSpec$file_out,
                    overwrite = TRUE,
                    datatype = "INT1U")
      } else {
        writeRaster(x = rast(thisLocSpec$file),
                    filename = thisLocSpec$file_out,
                    overwrite = TRUE,
                    datatype = "FLT4S")
        oldPref <- list.files(path = root,
                              pattern = first(str_split(last(str_split(string = thisLocSpec$file, "/")[[1]]), "[.]")[[1]]),
                              full.names = TRUE)
        file.remove(oldPref)
      }

    }

    opts$value[opts$var == "loc_pref"] <- paste0(c(opts$value[opts$var == "loc_pref"], .locspec$weight), collapse = "\t")
  }


  ## main.1 ----
  # main <- opts_old$tables$main

  ### integrate somewhere ----
  # # from suitability
  # opts_old$tables$main["value"][which(opts_old$tables$main$var == "drivers_max"),] <-
  #   as.character(as.numeric(opts_old$tables$main["value"][which(opts_old$tables$main$var == "drivers_max"),]) + newDriversN) # newDriversN = how many new, relevant if dynamic driver involved
  # opts_old$tables$main["value"][which(opts_old$tables$main$var == "drivers_tot"),] <-
  #   as.character(as.numeric(opts_old$tables$main["value"][which(opts_old$tables$main$var == "drivers_tot"),]) + newDriversN)
  #
  # # from conversion
  # opts_old$tables$main["value"][which(opts_old$tables$main$var == "drivers_max"),] <-
  #   as.character(as.numeric(opts_old$tables$main["value"][which(opts_old$tables$main$var == "drivers_max"),]) + 1)
  # opts_old$tables$main["value"][which(opts_old$tables$main$var == "drivers_tot"),] <-
  #   as.character(as.numeric(opts_old$tables$main["value"][which(opts_old$tables$main$var == "drivers_tot"),]) + 1)
  #
  # if(any(!is.na(opts_old$attributes$landuse[["preference"]]))){
  #   opts_old$tables$main["value"][which(opts_old$tables$main$var == "loc_pref"),] <- paste(c("1", paste(opts_old$attributes$landuse[["preference"]], collapse = "\t")), collapse = "\t")
  # }
  # opts_old$tables$main["value"][which(opts_old$tables$main$var == "ls_types"),] <- paste(opts_old$attributes$landuse[["ID"]], collapse = "\t")
  # opts_old$tables$main["value"][which(opts_old$tables$main$var == "ls_types_n"),] <- as.character(length(opts_old$attributes$landuse$system))
  # opts_old$tables$main["value"][which(opts_old$tables$main$var == "demand_type"),] <- paste(opts_old$attributes$demand[["match"]][opts_old$attributes$demand$type %in% .commodities], collapse = " ")
  # opts_old$tables$main["value"][which(opts_old$tables$main$var == "demand_types_n"),] <- as.character(length(.commodities))
  # opts_old$tables$main["value"][which(opts_old$tables$main$var == "resistance"),] <- paste(opts_old$attributes$landuse[["resistance"]], collapse = "\t")
  # prefs <- opts_old$attributes$landuse[["ID"]][opts_old$attributes$landuse$preference != 0] + 100
  # if(any(!is.na(prefs))){
  #   opts_old$tables$main["value"][which(opts_old$tables$main$var == "drivers_dyn"),] <- paste(c(length(prefs), prefs), collapse = "\t")
  # } else {
  #   opts_old$tables$main["value"][which(opts_old$tables$main$var == "drivers_dyn"),] <- as.character(0)
  # }
  # outMain <- opts_old$tables$main
  # assertTibble(x = outMain, nrows = 24, ncols = 2, types = c("character", "character"), any.missing = FALSE)
  #
  # opts_old$meta$file <- outMain
  ### integrate somewhere ----

  # outMain <- opts_old$meta$file |>
  #   pull(value) |>
  #   paste(collapse = "\n") |>
  #   writeLines(paste0(opts_old$path$model, "/main.1"))
  # writeLines(outMain, paste0(opts_old$path$model, "/main.1"))






  # test items ----

  ## general tests
  # - ensure that 'landtypes$suitability$driver' is a valid driver
  # - ensure that 'landtypes$production$crop' is a valid crop
  # - test for each commodity that demand$year has the same years as defined in the meta data and report missing years or filter years not in the meta data and report this
  # - test for each commodity that demand$region has the same values as defined in the slot regions and report missing regions or filter regions not in the slot and report this
  # - ensure that 'theRegion', which is used as mask, has the project crs

  # - test that the combined demand is not larger than the amount of pixels
  # maxDemand <- demand |>
  #   rowwise() |>
  #   mutate(sum = sum(across(all_of(.commodities))))
  # if(any(maxDemand$sum > opts_old$meta$cells)){
  #   stop("the overall demand for '", paste(demand$year[which(maxDemand$sum > opts_old$meta$cells)], collapse = ", "),"' is larger than the number of cells (", opts_old$meta$cells, ").")
  # }
  #
  # # test that all project years are in the demand table
  # aimYears <- seq(from = opts_old$meta$first_year, to = opts_old$meta$last_year, by = 1)
  # actualYears <- unique(opts_old$data$demand$year)
  # if(!all(aimYears %in% actualYears)){
  #   stop(paste0("not all project years are in the demand table (", paste0(aimYears[which(!aimYears %in% actualYears)], collapse = ", "), ")\n   -> please either provide the missing demand or adapt the project years."))
  # }
  # - test dimensions of rasters perhaps
  # theDims <- dim(theRaster)
  # if(any(theDims[1] > 4000 | theDims[2] > 4000)) {
  #   stop("this gridded object is too large!")
  # }
  # - test the crs and extent as well

  ## demand.in
  # if(dim(.demand)[1] == 0) {
  #   stop("after filtering for target years and commodities, the tables doesn't contain any entries anymore.")
  # }
  #
  # availComm <- opts_old$attributes$demand$type %in% unique(demand$commodity)
  # demand <- demand |>
  #   dplyr::select(c("year", "commodity", region, all_of(values))) |>
  #   pivot_wider(names_from = commodity, values_from = values) |>
  #   rename(region = region) |>
  #   arrange(year) |>
  #   dplyr::select(region, year, opts_old$attributes$demand$type[availComm])
  #
  # change <- demand |>
  #   dplyr::select(all_of(.commodities)) |>
  #   mutate(across(.cols = everything(),
  #                 .fns = list(diff2),
  #                 .names = "{col}")) |>
  #   mutate(across(.cols = everything(),
  #                 .fns = list(cumsum),
  #                 .names = "{col}"))
  #
  # - test for each actual demand a couple of things
  # attrib <- opts_old$attributes$landuse
  # conv <- as.numeric(str_split(main[["value"]][14], "\t")[[1]])
  # for(i in .commodities){
  #
  #   # determine the landuse types that produce this demand
  #   prodTypes <- production |>
  #     filter(!!sym(i) != 0)
  #
  #   # test that the demand has a production specification
  #   if(dim(prodTypes)[1] == 0){
  #     stop("'", i, "' is not produced by any landuse type.")
  #   }
  #
  #   # get first years demand
  #   firstDemand <- initialDemand[[i]]
  #
  #   # determine the total amount of the already produced demand
  #   prodDemand <- left_join(prodTypes, attrib, by = "system")
  #   prodDemand <- sum(prodDemand[[i]] * prodDemand[["cells"]])
  #
  #   # determine the landuse types this landuse type can be converted to and
  #   # from, and the number of cells each of the allowed landuse types has in the
  #   # initial landuse map (cov_all.0)
  #   allowedTo <- rules |>
  #     filter(system %in% prodTypes$system) |>
  #     pivot_longer(-system, names_to = "to", values_to = "allowed") |>
  #     filter(allowed != 0) |>
  #     left_join(attrib, by = c("to" = "system"))
  #   allowedFrom <- rules |>
  #     dplyr::select("system", prodTypes$system) |>
  #     pivot_longer(-system, names_to = "to", values_to = "allowed") |>
  #     filter(allowed != 0) |>
  #     left_join(attrib, by = "system")
  #
  #   if(dim(allowedFrom)[1] == 1){
  #     reason1 <- paste0("\n  --> no landuse type can be converted to '", paste(allowedFrom$to, collapse = ", "), "', producing '", i, "'.")
  #   } else {
  #     reason1 <- NULL
  #   }
  #   if(dim(allowedTo)[1] == 1){
  #     reason2 <- paste0("\n  --> '", paste(allowedTo$system, collapse = ", "), "', producing '", i, "', can't be converted to another landuse type.")
  #   } else {
  #     reason2 <- NULL
  #   }
  #
  #   # determine the number of cells that are not already in use for demand
  #   # production, but which are allowed to produce more (i.e., which the target
  #   # landuse are allowed to be converted to or from due to the conversion
  #   # rules)
  #   remainingTo <- sum(allowedTo$cells) - prodDemand
  #   remainingFrom <- sum(allowedFrom$cells) - prodDemand
  #
  #   # go through each demand change and check whether it's higher or lower than the remaining pixels
  #   for(j in seq_along(change[[i]])[-1]){
  #
  #     thisChange <- change[[j,i]]
  #
  #     if(thisChange > 0){
  #       if(thisChange > remainingFrom){
  #         stop("For ", demand$year[j], " CLUMondo will try to allocate more '", i, "' (", thisChange, ") than allowed (", remainingFrom, ").", reason1)
  #       }
  #     } else if(thisChange < 0){
  #       if(abs(thisChange) > remainingTo){
  #         stop("For ", demand$year[j], " CLUMondo will try to allocate less '", i, "' (", thisChange, ") than allowed (", remainingTo, ").", reason2)
  #       }
  #     }
  #
  #   }
  #
  #   # test that demand of the first year is actually the same as pixels in the
  #   # intial landuse multiplied with production amount.
  #   if(firstDemand != prodDemand){
  #     warning("The target demand (", firstDemand, ") and the produced demand (", prodDemand, ") of '", i, "' are not equal.")
  #   }
  #
  #   # test that first and second convergence criteria are below the rate of
  #   # change of demand.
  #   testDemand <- demand |>
  #     dplyr::select(all_of(i))
  #   testDemand <- tibble(first = testDemand[[1]][-dim(testDemand)[1]], second = testDemand[-1,][[1]]) |>
  #     mutate(dev = abs((first/second)*100 - 100))
  #
  #   if(any(testDemand$dev < conv[2])){
  #     warning("The first convergence criterion (", conv[2], ") is larger than the smallest amount of change in demand of '", i, "' (", round(min(testDemand$dev, na.rm = TRUE), 3), ").")
  #   }
  #   if(any(testDemand$dev < conv[3])){
  #     warning("The second convergence criterion (", conv[3], ") is larger than the smallest amount of change in demand of '", i, "' (", round(min(testDemand$dev, na.rm = TRUE), 3), ").")
  #   }
  #
  # }

  ## alloc1.reg
  # assertSubset(x = names(suitability)[-which(names(suitability) == "const")], choices = names(scene@drivers))
  # assertTibble(x = outAlloc1, nrows = length(opts_old$attributes$landuse$system))
  # assertNames(x = names(outAlloc1), must.include = c("system", "const", "alloc"))
  # test that each landuse type has a suitability specification
  # nonValid <- suit |>
  #   filter(is.na(const))
  # if(dim(nonValid)[1] != 0){
  #   stop("the landuse type '", paste(nonValid$system, collapse = ", "), "' does not have a valid suitability specification.")
  # }

  ## alloc2.reg
  # assertTibble(x = outAlloc2, nrows = length(opts_old$attributes$landuse$system))
  # assertNames(x = names(outAlloc2), must.include = c("system", "const", "alloc", "neighmat", "weight"))
  # test that each landuse type has a neighbourhood specification
  # if(opts_old$tables$main[which(opts_old$tables$main$var == "neigh"),]$value == 1){
  #   nonValid <- neigh |>
  #     filter(is.na(const))
  #   if(dim(nonValid)[1] != 0){
  #     stop("the landuse type '", paste(nonValid$system, collapse = ", "), "' does not have a valid neighbourhood specification.")
  #   }
  # }

  ## neighmat.txt

  ## allow.txt
  # test that each landuse type has a conversion rule specified at least to itself
  # nonValid <- rules |>
  #   filter(across(where(is.numeric), ~ . == 0))
  # if(dim(nonValid)[1] != 0){
  #   stop("the landuse type '", paste(nonValid$system, collapse = ", "), "' does not have a valid set of conversion rules.")
  # }

  ## lusconv.txt

  ## lusmatrix.txt



  # implement stepwise modifier ----
  # if(stepwise | opts_old$meta$stepwise){
  #
  #   # pull in the required data ----
  #   theSteps <- opts_old$data$demand$year[-length(opts_old$data$demand$year)]
  #   for(i in seq_along(theSteps)){
  #
  #     theYears <-  opts_old$data$demand$year[c(i, i+1)]
  #
  #     # create first and second year input files
  #     if(i == 1){
  #       first <- list.files(path = paste0(opts_old$path$model, "/input"), pattern = paste0("cov_all.0|[.]fil[.]"))
  #       file.copy(from = paste0(opts_old$path$model, "/input/", first),
  #                 to = paste0(opts_old$path$model, "/", first),
  #                 overwrite = TRUE)
  #     } else {
  #       first <- c(paste0("age.0.asc"), paste0("cov_all.0.asc"))
  #       second <- c(paste0("age.1.asc"), paste0("cov_all.1.asc"))
  #       file.copy(from = paste0(opts_old$path$model, "/", second),
  #                 to = paste0(opts_old$path$model, "/", first),
  #                 overwrite = TRUE)
  #       file.remove(paste0(opts_old$path$model, "/", second))
  #     }
  #
  #     # run the step modifier ----
  #     if(length(opts_old$modifier) != 0){
  #       for(j in seq_along(opts_old$modifier)){
  #         do.call(what = paste0("mod_", names(opts_old$modifier)[j]), args = list())
  #       }
  #     }
  #
  #     # write the file demand1.in ----
  #     demand <- opts_old$data$demand %>%
  #       filter(year %in% theYears) %>%
  #       dplyr::select(-region, -year) %>%
  #       unite(col = "string", sep = "\t") %>%
  #       unlist()
  #     paste0(length(demand), "\n", paste0(demand, collapse = "\n")) %>%
  #       writeLines(paste0(opts_old$path$model, "/demand.in1"))
  #
  #     # write the file main.1 ----
  #     opts_old <- getOption(x = "lclu")
  #     outMain <- opts_old$meta$file
  #     outMain[["value"]][outMain$var == "years"] <- paste(theYears, collapse = "\t")
  #     outMain %>%
  #       pull(value) %>%
  #       paste(collapse = "\n") %>%
  #       writeLines(paste0(opts_old$path$model, "/main.1"))
  #
  #   }
  # }


  # return(log) produce an error log from the tests and return that instead (with message())
}
