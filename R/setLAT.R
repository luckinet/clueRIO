#' Set a raster attribute table of the landuse grid (LAT)
#'
#' Land-use attribute table
#' @param table [`data.frame(1)`][data.frame]\cr table that contains the columns
#'   \code{'system'} and \code{'ID'} of the unique values in the initial landuse
#'   map.
#' @family functions to specify gridded objects
#' @importFrom checkmate assertDataFrame assertNames testTRUE
#' @importFrom raster raster values
#' @importFrom tibble tibble
#' @importFrom dplyr left_join
#' @export

setLAT <- function(table = NULL) {

  .Deprecated("clue_landsystems")

  opts <- getOption("lclu")

  assertDataFrame(x = table, min.cols = 2)
  assertNames(x = names(table), must.include = c("ID", "system"))

  # this should also include a couple of checks, whether the values in the table
  # are actually correct.
  initial <- opts$files$cov_all.0.asc$obj
  theCells <- table(values(initial))
  cellValues <- as.numeric(names(theCells))

  assertTRUE(x = all(table$ID %in% cellValues))
  assertTRUE(x = all(cellValues %in% table$ID))

  # ensure that all landuse types have their amount in the attribute table
  table <- left_join(table, opts$attributes$landuse, by = "system")
  theCells <- tibble(cells = theCells, ID = cellValues)
  theRAT <- left_join(x = table, y = theCells, by = "ID")

  # ensure that all landuse types in the intial landuse map have an entry in the
  # attribute table
  attribInCells <- testTRUE(all(cellValues %in% theRAT$ID))
  if(!attribInCells){
    stop("the landuse attribute table (LAT) does not contain all classes in the intial landuse map.")
  }
  cellsInAttrib <- testTRUE(all(theRAT$ID %in% cellValues))
  if(!cellsInAttrib){
    warning("the initial landuse map does not contain all target landuse classes.")
  }

  opts$attributes$landuse <- theRAT

  options(lclu = opts)

}
