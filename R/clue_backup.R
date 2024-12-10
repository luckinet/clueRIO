#' Backup the CLUE scene
#'
#' @param scene [`scene(1)`][scene]\cr the CLUE scene to back up.
#' @details
#' @return
#' @examples
#' @importFrom checkmate assertClass assertFileExists assertCharacter
#' @importFrom dplyr first last
#' @importFrom stringr str_split
#' @importFrom terra writeRaster
#' @export

clue_backup <- function(scene){

  # current <- list.files(path = opts$path$model)
  # if(any(current %in% c("log.fil"))){
  #   theTime <- paste0(strsplit(x = format(Sys.time(), format="%Y%m%d_%H%M%S"), split = "[ ]")[[1]], collapse = "_")
  #   backupFiles <- current[grep("log.fil", current)]
  #   file.copy(from = paste0(opts$path$model, "/", backupFiles), to = paste0(opts$path$model, "/backup/", paste0(theTime, "_", backupFiles)))
  # }
  #
  # # remove old error reports ----
  # toRemove <- current[grep("error", current)]
  # if(length(toRemove) != 0){
  #   file.remove(paste0(opts$path$model, "/", toRemove))
  # }

  # if(backup){
  #   current <- list.files(path = opts$path$model)
  #   oldMeta <- read_rds(file = paste0(opts$path$model, "/meta.rds"))
  #   covs <- grepl(pattern = "cov_all", x = current)
  #   ages <- grepl(pattern = "age", x = current)
  #   theLog <- grepl(pattern = "log.fil", x = current)
  #   theMeta <- grepl(pattern = "meta.rds", x = current)
  #   toMove <- current[c(covs | ages | theLog | theMeta)]
  #   suppressMessages(file.copy(paste0(opts$path$model, "/", toMove), paste0(opts$path$root, "/processing/", paste0(region, "_", toMove)), overwrite = TRUE))
  # }
}
