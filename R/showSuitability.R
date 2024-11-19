#' Plot of suitabilities
#'
#' This function shows suitability maps by focusing on a 'target' driver and the
#' combination of all 'comparison' drivers.
#' @param target [`character(1)`][character]\cr the focal driver.
#' @param comparison [`character(.)`][character]\cr the drivers to compare the
#'   focal driver with.
#' @param zlim [`numeric(2)`][numeric]\cr
#' @export

showSuitability <- function(target = NULL, comparison = NULL, zlim = c(0, 1), steps = 100){

  opts <- getOption("lclu")
  alloc <- opts$tables$alloc1 %>%
    dplyr::select(-alloc, -const)

  # test that 'target' and 'comparison' is a valid landuse system
  targetDrivers <- opts$attributes$gridded$driver
  assertChoice(x = target, choices = targetDrivers)
  assertChoice(x = comparison, choices = targetDrivers, null.ok = TRUE)

  # determine item names ...
  focal <- opts$attributes$gridded$file[opts$attributes$gridded$driver == target]
  if(is.null(comparison)){
    temp <- list.files(path = paste0(opts$path$model, "/input"), pattern = "sc1gr")
    temp <- temp[-which(temp == focal)]
  } else {
    temp <- opts$attributes$gridded$file[opts$attributes$gridded$driver %in% comparison]
  }
  theNames <- opts$attributes$gridded$driver[opts$attributes$gridded$file %in% temp]

  # ... and load them
  focal <- raster(x = paste0(opts$path$model, "/input/", focal))
  others <- stack(x = paste0(opts$path$model, "/input/", temp))
  # maximum probability for allocation is presumably where suitability of the
  # focal class is highest AND where suitability of the other classes is lowest,
  # so I subtract the others from the focal
  multiplied <- lapply(1:dim(others)[3], function(x){
    focal - others[[x]]
  })
  out <- stack(multiplied)
  summarised <- max(out)
  out <- stack(out, summarised)
  names(out) <- c(paste0("d_", theNames), "maximum")

  plot(out, zlim = zlim, col = hcl.colors(n = steps, palette = "Lajolla"))
}
