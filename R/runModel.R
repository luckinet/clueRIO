#' Run the model
#'
#' @param mode [`integerish(1)`][integer]\cr the model mode.
#' @param backup [`logical(1)`][logical]\cr whether or not the output of the
#'   previous run shall be stored in a folder where it is not overwritten.
#' @details This function creates a meta-data object in the main directory,
#'   calls \code{\link{makeInput}} to build all the input files required by
#'   CLUMondo and sends a call to the OS console to run CLUMondo first to create
#'   probability maps, then to allocate.
#' @return No return value, called for the side effect of preparing a CLUMondo
#'   model and running it.
#' @importFrom checkmate assertLogical assertIntegerish assertFileExists
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr pull filter select
#' @importFrom stringr str_replace
#' @importFrom tidyr unite
#' @export

runModel <- function(region = NULL, mode = NULL, stepwise = FALSE, backup = FALSE){

  opts <- getOption(x = "lclu")
  assertLogical(x = backup, len = 1)
  assertLogical(x = stepwise, len = 1)
  assertIntegerish(x = mode, lower = 1, upper = 5, null.ok = TRUE)

  # test whether CLUMondo.exe is available ----
  assertFileExists(x = paste0(opts$path$model, "/CLUMondo.exe"))

  # backup metadata and previous log ----
  current <- list.files(path = opts$path$model)
  if(any(current %in% c("log.fil"))){
    theTime <- paste0(strsplit(x = format(Sys.time(), format="%Y%m%d_%H%M%S"), split = "[ ]")[[1]], collapse = "_")
    backupFiles <- current[grep("log.fil", current)]
    file.copy(from = paste0(opts$path$model, "/", backupFiles), to = paste0(opts$path$model, "/backup/", paste0(theTime, "_", backupFiles)))
  }
  # remove old error reports
  toRemove <- current[grep("error", current)]
  if(length(toRemove) != 0){
    file.remove(paste0(opts$path$model, "/", toRemove))
  }

  # set model mode ----
  if(!is.null(mode)){
    message("  --> running the model with mode ", mode)
    model_mode <- paste0(" ", mode)
  } else {
    model_mode <- NULL
  }

  if(stepwise | opts$meta$stepwise){

    # pull in the required data ----
    theSteps <- opts$data$demand$year[-length(opts$data$demand$year)]
    for(i in seq_along(theSteps)){

      theYears <-  opts$data$demand$year[c(i, i+1)]

      # create first and second year input files
      if(i == 1){
        first <- list.files(path = paste0(opts$path$model, "/input"), pattern = paste0("cov_all.0|[.]fil[.]"))
        file.copy(from = paste0(opts$path$model, "/input/", first),
                  to = paste0(opts$path$model, "/", first),
                  overwrite = TRUE)
      } else {
        first <- c(paste0("age.0.asc"), paste0("cov_all.0.asc"))
        second <- c(paste0("age.1.asc"), paste0("cov_all.1.asc"))
        file.copy(from = paste0(opts$path$model, "/", second),
                  to = paste0(opts$path$model, "/", first),
                  overwrite = TRUE)
        file.remove(paste0(opts$path$model, "/", second))
      }

      # run the step modifier ----
      if(length(opts$modifier) != 0){
        for(j in seq_along(opts$modifier)){
          do.call(what = paste0("mod_", names(opts$modifier)[j]), args = list())
        }
      }

      # write the file demand1.in ----
      demand <- opts$data$demand %>%
        filter(year %in% theYears) %>%
        dplyr::select(-region, -year) %>%
        unite(col = "string", sep = "\t") %>%
        unlist()
      paste0(length(demand), "\n", paste0(demand, collapse = "\n")) %>%
        writeLines(paste0(opts$path$model, "/demand.in1"))

      # write the file main.1 ----
      opts <- getOption(x = "lclu")
      outMain <- opts$meta$file
      outMain[["value"]][outMain$var == "years"] <- paste(theYears, collapse = "\t")
      outMain %>%
        pull(value) %>%
        paste(collapse = "\n") %>%
        writeLines(paste0(opts$path$model, "/main.1"))

      # run the model ----
      message("\nrunning CLUMondo for the years ", theYears[1], " - ", theYears[2], ".")
      if(Sys.info()[['sysname']] == "Linux"){
        system(paste0("cd ", opts$path$model, "\n wine CLUMondo.exe demand.in1 region.fil.asc", model_mode))
      } else {
        system(paste0("cd ", opts$path$model, "\n CLUMondo.exe demand.in1 region.fil.asc", model_mode))
      }

      # push data to output ----
      if(i == 1){
        first <- list.files(path = paste0(opts$path$model), pattern = paste0("cov_all.0.asc|age.0.asc"))
        file.copy(from = paste0(opts$path$model, "/", first),
                  to = paste0(opts$path$model, "/output/", first),
                  overwrite = TRUE)
      }
      output <- list.files(path = paste0(opts$path$model), pattern = paste0("cov_all.1.asc|age.1.asc"))
      outNames <- c(paste0("age.", i, ".asc"), paste0("cov_all.", i, ".asc"))
      file.copy(from = paste0(opts$path$model, "/", output),
                to = paste0(opts$path$model, "/output/", outNames),
                overwrite = TRUE)

    }

  } else {

    # pull in the required data ----
    gridded <- list.files(path = paste0(opts$path$model, "/input"))
    file.copy(from = paste0(opts$path$model, "/input/", gridded),
              to = paste0(opts$path$model, "/", gridded),
              overwrite = TRUE)

    # make the file demand1.in ----
    demand <- opts$data$demand %>%
      dplyr::select(-region, -year) %>%
      unite(col = "string", sep = "\t") %>%
      unlist()
    paste0(length(demand), "\n", paste0(demand, collapse = "\n")) %>%
      writeLines(paste0(opts$path$model, "/demand.in1"))

    # write the file main.1 ----
    opts$meta$file %>%
      pull(value) %>%
      paste(collapse = "\n") %>%
      writeLines(paste0(opts$path$model, "/main.1"))

    # run the model ----
    message("\nrunning CLUMondo")
    if(Sys.info()[['sysname']] == "Linux"){
      system(paste0("cd ", opts$path$model, "\n wine CLUMondo.exe demand.in1 region.fil.asc", model_mode))
    } else {
      system(paste0("cd ", opts$path$model, "\n CLUMondo.exe demand.in1 region.fil.asc", model_mode))
    }

    # push data to output ----
    output <- list.files(path = paste0(opts$path$model), pattern = "age|cov_all|landarea|outputmatrix")
    file.copy(from = paste0(opts$path$model, "/", output),
              to = paste0(opts$path$model, "/output/", output),
              overwrite = TRUE)

  }


  if(backup){
    current <- list.files(path = opts$path$model)
    oldMeta <- read_rds(file = paste0(opts$path$model, "/meta.rds"))
    covs <- grepl(pattern = "cov_all", x = current)
    ages <- grepl(pattern = "age", x = current)
    theLog <- grepl(pattern = "log.fil", x = current)
    theMeta <- grepl(pattern = "meta.rds", x = current)
    toMove <- current[c(covs | ages | theLog | theMeta)]
    suppressMessages(file.copy(paste0(opts$path$model, "/", toMove), paste0(opts$path$root, "/processing/", paste0(region, "_", toMove)), overwrite = TRUE))
  }

}
