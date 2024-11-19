#' Build all files required for the model
#'
#' This function generates the tables that are needed to run CLUMondo.
#' @return No return value, called for the side effect of creating all the basic
#'   files needed to run CLUMondo (such as \emph{cov_all.0.asc},
#'   \emph{demand.in1}, \emph{allc1.reg} or \emph{main.1}, etc) in the project
#'   directory.
#' @importFrom checkmate assertTibble assertNames assertNumeric assertSubset
#' @importFrom dplyr select pull
#' @importFrom stringr str_count
#' @importFrom tidyr unite
#' @importFrom tidyselect all_of
#' @importFrom readr write_delim
#' @export

makeModel <- function(stepwise = FALSE){

  opts <- getOption("lclu")

  # test the model ----
  testModel()


  # remove previous files ----
  main <- list.files(path = opts$path$model)
  main <- main[-grep("backup", main)]
  main <- main[-grep("input", main)]
  main <- main[-grep("output", main)]
  main <- main[-grep("CLUMondo.exe", main)]
  if(length(main) != 0){
    file.remove(paste0(opts$path$model, "/", main))
  }
  # inputs <- list.files(path = paste0(opts$path$model, "/input"))
  # outputs <- list.files(path = paste0(opts$path$model, "/output"))
  # if(length(inputs) != 0){
  #   file.remove(paste0(opts$path$model, "/input/", inputs))
  # }
  # if(length(outputs) != 0){
  #   file.remove(paste0(opts$path$model, "/output/", outputs))
  # }


  message(" ... writing tables")

  # make the file alloc1.reg and alloc2.reg ----
  outAlloc1 <- opts$tables$alloc1
  assertTibble(x = outAlloc1, nrows = length(opts$attributes$landuse$system))
  assertNames(x = names(outAlloc1), must.include = c("system", "const", "alloc"))
  outAlloc1 <- paste(outAlloc1$alloc, collapse = "\n")
  writeLines(outAlloc1, paste0(opts$path$model, "/alloc1.reg"))

  if(opts$tables$main[which(opts$tables$main$var == "neigh"),]$value == 1){
    outAlloc2 <- opts$tables$alloc2
    assertTibble(x = outAlloc2, nrows = length(opts$attributes$landuse$system))
    assertNames(x = names(outAlloc2), must.include = c("system", "const", "alloc", "neighmat"))
    outNeighmat <- paste0(paste0(outAlloc2$weight, collapse = " "), "\n\n", paste(outAlloc2$neighmat, collapse = "\n"))
    outAlloc2 <- paste(outAlloc2$alloc, collapse = "\n")
    writeLines(outAlloc2, paste0(opts$path$model, "/alloc2.reg"))
    writeLines(outNeighmat, paste0(opts$path$model, "/neighmat.txt"))
  }


  # make the file allow.txt ----
  outAllow <- opts$tables$allow
  assertTibble(x = outAllow, nrows = length(opts$attributes$landuse$system), ncols = length(opts$attributes$landuse$system)+1)
  assertNames(x = names(outAllow), must.include = "system")

  outAllow <- outAllow %>%
    dplyr::select(-system)
  write_delim(x = as_tibble(outAllow, .name_repair = "minimal"), path = paste0(opts$path$model, "/allow.txt"), col_names = FALSE)


  # get the actual demands ----
  outDemand <- opts$data$demand
  actualDemand <- outDemand %>%
    dplyr::select(-region, -year) %>%
    names()

  assertTibble(x = outDemand, ncols = length(actualDemand)+2)
  assertNames(x = names(outDemand), identical.to = c("region", "year", actualDemand))


  # make the file main.1 ----
  if(any(!is.na(opts$attributes$landuse[["preference"]]))){
    opts$tables$main["value"][which(opts$tables$main$var == "loc_pref"),] <- paste(c("1", paste(opts$attributes$landuse[["preference"]], collapse = "\t")), collapse = "\t")
  }
  opts$tables$main["value"][which(opts$tables$main$var == "ls_types"),] <- paste(opts$attributes$landuse[["ID"]], collapse = "\t")
  opts$tables$main["value"][which(opts$tables$main$var == "ls_types_n"),] <- as.character(length(opts$attributes$landuse$system))
  opts$tables$main["value"][which(opts$tables$main$var == "demand_type"),] <- paste(opts$attributes$demand[["match"]][opts$attributes$demand$type %in% actualDemand], collapse = " ")
  opts$tables$main["value"][which(opts$tables$main$var == "demand_types_n"),] <- as.character(length(actualDemand))
  opts$tables$main["value"][which(opts$tables$main$var == "resistance"),] <- paste(opts$attributes$landuse[["resistance"]], collapse = "\t")
  prefs <- opts$attributes$landuse[["ID"]][opts$attributes$landuse$preference != 0] + 100
  if(any(!is.na(prefs))){
    opts$tables$main["value"][which(opts$tables$main$var == "drivers_dyn"),] <- paste(c(length(prefs), prefs), collapse = "\t")
  } else {
    opts$tables$main["value"][which(opts$tables$main$var == "drivers_dyn"),] <- as.character(0)
  }
  outMain <- opts$tables$main
  assertTibble(x = outMain, nrows = 24, ncols = 2, types = c("character", "character"), any.missing = FALSE)
  assertNames(x = names(outMain), identical.to = c("var", "value"))

  opts$meta$file <- outMain


  # make the file lusconv.txt ----
  outConv <- opts$tables$lusconv
  assertTibble(x = outConv, nrows = length(opts$attributes$landuse$system), ncols = length(opts$attributes$demand$type)+1)
  assertNames(x = names(outConv), must.include = "system")

  outConv <- outConv %>%
    dplyr::select(all_of(actualDemand))
  write_delim(x = as_tibble(outConv, .name_repair = "minimal"), path = paste0(opts$path$model, "/lusconv.txt"), col_names = FALSE)


  # make the file lusmatrix.txt ----
  outMat <- opts$tables$lusmatrix
  assertTibble(x = outMat, nrows = length(opts$attributes$landuse$system), ncols = length(opts$attributes$demand$type)+1)
  assertNames(x = names(outMat), must.include = "system")

  outMat <- outMat %>%
    dplyr::select(all_of(actualDemand))
  write_delim(x = as_tibble(outMat, .name_repair = "minimal"), path = paste0(opts$path$model, "/lusmatrix.txt"), col_names = FALSE)


  message(" ... writing gridded objects")

    # make region.fil ----
  if (!any(opts$attributes$gridded$driver %in% "region")) {
    thePath <- paste0(opts$path$model, "/input/region.fil.asc")
    region <- opts$files$cov_all.0.asc$obj
    region[region > 0] <- 0
    region@file@nodatavalue <- -9999
  } else {
    region <- opts$files$region.fil.asc$obj
  }
  writeRaster(x = region,
              filename = thePath,
              overwrite = TRUE,
              format = "ascii",
              datatype = "INT1U")

  for(i in seq_along(opts$files)){

    writeRaster(x = opts$files[[i]]$obj,
                filename = paste0(opts$path$model, "/input/", names(opts$files)[i]),
                overwrite = TRUE,
                format = "ascii",
                datatype = opts$files[[i]]$type)

  }



  # make a file of the meta-data ----
  # saveRDS(object = opts, file = paste0(opts$path$model, "/meta.rds"))


  options(lclu = opts)

}
