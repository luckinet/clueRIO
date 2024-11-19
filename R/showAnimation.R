#' Animate landuse change
#'
#' @param interval [`numeric(1)`][numeric]\cr time increment of the animation.
#' @param input `logical(1)`][logical]\cr whether or not to also include the
#'   gridded objects (sc1gr*.fil) in the animation.
#' @importFrom stringr str_extract str_extract_all
#' @importFrom readr read_rds
#' @importFrom raster stack plot
#' @importFrom animation saveHTML
#' @importFrom viridisLite cividis
#' @export

showAnimation <- function(interval = 1, input = FALSE, target = "cover"){

  assertChoice(x = target, choices = c("cover", "age"))
  opts <- getOption("lclu")
  if(target == "cover"){
    target <- "cov"
    target2 <- "age.0"
    targetTitle <- "initial landuse"
    targetTitle2 <- "initial age"
  } else {
    target <- "age"
    target2 <- "cov_all.0"
    targetTitle <- "initial age"
    targetTitle2 <- "initial landuse"
  }
  # meta <- read_rds(path = paste0(opts$path$model, "/meta.rds"))

  temp <- list.files(path = paste0(opts$path$model, "/output"), pattern = target)

  sortBy <- as.integer(str_extract(string = temp, pattern = "[[:digit:]]+"))
  covers <- list.files(path = paste0(opts$path$model, "/output"), pattern = target, full.names = TRUE)[order(sortBy)]
  if(any(grepl("aux.xml", covers))) covers <- covers[-grep("aux.xml", covers)]

  toAnimate <- covers
  titles <- c(targetTitle, (opts$meta$first_year:opts$meta$last_year)[-1])[1:length(toAnimate)]
  if(target == "cov"){
    newLegend <- rep(TRUE, length(titles))
  } else {
    newLegend <- rep(FALSE, length(titles))
  }

  if(input){
    temp <- list.files(path = opts$path$model, pattern = "sc1gr\\d.fil")
    if(any(grepl("aux.xml", temp))) temp <- temp[-grep("aux.xml", temp)]

    sortBy <- as.integer(str_extract_all(string = temp, pattern = "[[:digit:]]+", simplify = TRUE)[,2])
    gridded <- list.files(path = opts$path$model, pattern = "sc1gr\\d.fil", full.names = TRUE)[order(sortBy)]
    if(any(grepl("aux.xml", gridded))) gridded <- gridded[-grep("aux.xml", gridded)]

    region <- list.files(path = opts$path$model, pattern = "region", full.names = TRUE)
    if(any(grepl("aux.xml", region))) region <- region[-grep("aux.xml", region)]

    temp2 <- list.files(path = paste0(opts$path$model, "/output"), pattern = target2, full.names = TRUE)
    if(any(grepl("aux.xml", temp2))) temp2 <- temp2[-grep("aux.xml", temp2)]

    gridded <- c(gridded, region, temp2)
    toAnimate <- c(gridded, covers)
    titles <- c(opts$attributes$gridded$driver[order(temp)], "region", targetTitle2, titles)
    newLegend <- c(rep(FALSE, length(opts$attributes$gridded$driver)+2), newLegend)
  }

  objs <- stack(toAnimate)

  oldWD <- getwd()
  setwd(paste0(opts$path$model, "/backup")) # didn't find an option to set the path in saveHTML

  saveHTML(
    expr = {
      for (i in seq_along(titles)){

        if(newLegend[i]){
          lus <- opts$attributes$landuse$system
          cols <- hcl.colors(n = length(lus), palette = "Cividis")
          plot(objs[[i]], main = titles[i], col = cols, legend = FALSE)
          legend("bottomright", legend = lus, fill = cols)
        } else {
          plot(objs[[i]], main = titles[i])
        }

      }
    },
    interval = interval,
    title = opts$meta$name,
    img.name = paste0(opts$meta$name, "_", opts$meta$time),
    htmlfile = paste0(opts$meta$name, "_", opts$meta$time, ".html"))

  setwd(oldWD)
}