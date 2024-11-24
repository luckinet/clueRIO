#' Crop rotation modifier
#'
#' @details To implement crop rotations, \code{'locspec'} files are adapted for
#'   each year. The yearly \code{'locspec'} is calculated by \enumerate{\item
#'   multiplying the \code{'age'} layer with the \code{'locspec weight factor'}
#'   and \item selecting only a subset of pixels, depending on the sign of this
#'   weight factor. In case the sign is negative, it would keep those pixels
#'   that were covered by the focal class in the previous time-step and thus
#'   down-weigh where is was present previously. In case the sign is positive,
#'   it would keep those pixels that were covered by all other classes in the
#'   previous time-step and thus up-weigh where it was not present previously.}
#' @importFrom stringr str_split
#' @importFrom dplyr select

# mod_crop_rotation <- function(){
#
#   opts <- getOption("lclu")
#   alloc <- opts$tables$alloc1 %>%
#     dplyr::select(-alloc, -const)
#
#   pref <- opts$attributes$landuse$preference
#   syst <- opts$attributes$landuse$system
#   loc_prefs <- str_split(opts$tables$main[["value"]][opts$tables$main$var == "loc_pref"], "\t")[[1]]
#
#   cov_all <- raster(x = paste0(opts$path$model, "/cov_all.0.asc"))
#   cov_all <- setMinMax(cov_all)
#
#   if(!any(grepl(pattern = "age.0.asc", x = list.files(opts$path$model)))){
#
#     # make new age file based on previously set value of 'ls_hist'
#     age <- cov_all
#     maxAge <- as.integer(str_split(opts$tables$main[["value"]][opts$tables$main$var == "ls_hist"], "\t")[[1]][2])
#     age[] <- sample.int(n = maxAge, size = dim(age)[1]*dim(age)[2], replace = T)
#     writeRaster(age,
#                 filename = paste0(opts$path$model, "/age.0.asc"),
#                 overwrite = TRUE,
#                 format = "ascii",
#                 datatype = "INT1U")
#
#     # and also set the option so that this file is picked up by CLUMondo
#     opts$meta$file[["value"]][opts$meta$file$var == "ls_hist"] <- as.character(0)
#   } else {
#     age <- raster(x = paste0(opts$path$model, "/age.0.asc"))
#     age <- setMinMax(age)
#   }
#
#   for(i in seq_along(pref)){
#
#     if(pref[i] == 0){
#       next
#     }
#
#     suitLayer <- alloc[which(alloc$system == syst[i]),] %>%
#       select_if(. == 1)
#     if(length(suitLayer) > 1){
#       stop("no more than one suitability layer are allowed for '", syst[i], "' when using the 'crop_rotation' modifier.")
#     }
#     suitLayer <- opts$attributes$gridded$file[opts$attributes$gridded$driver == names(suitLayer)]
#     suitability <- raster(x = paste0(opts$path$model, "/", suitLayer))
#
#     if(pref[i] > 0){
#       loc_prefs[i+1] <- as.character(1)
#
#       temp <- age*abs(pref[i])
#       locspec <- temp*suitability
#       locspec[cov_all == i-1] <- 0
#     } else {
#       loc_prefs[i+1] <- as.character(-1)
#
#       temp <- age*abs(pref[i])
#       locspec <- temp*suitability
#       locspec[cov_all != i-1] <- 0
#     }
#
#     theValues <- values(x = locspec)
#     # if(min(theValues) < -1){
#     #   warning("some locations of 'LUS ", i-1, "' were older than implicitly allowed by 'weight'.")
#     # }
#     # if(max(theValues) > 1){
#     #   stop("make this error massage!")
#     # }
#
#     writeRaster(locspec,
#                 filename = paste0(opts$path$model, "/locspec", i-1, ".1.asc"),
#                 overwrite = TRUE,
#                 format = "ascii",
#                 datatype = "FLT4S")
#     writeRaster(locspec,
#                 filename = paste0(opts$path$model, "/locspec", i-1, ".fil.asc"),
#                 overwrite = TRUE,
#                 format = "ascii",
#                 datatype = "FLT4S")
#   }
#   opts$meta$file[["value"]][opts$meta$file$var == "loc_pref"] <- paste(loc_prefs, collapse = "\t")
#
#   options(lclu = opts)
#
# }
