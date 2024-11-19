#' Set a conversion rule
#'
#' This function constructs the conversion between two landuse types or a
#' commodity and a landuse type.
#' @param from [`character(1)`][character]\cr a landuse type for which a
#'   relationship shall be created.
#' @param to [`character(1)`][character]\cr a landuse type to which the
#'   relationship shall be created.
#' @param label [`character(1)`][character]\cr description of the relationship.
#' @param thresh [`integerish(1)`][integer]\cr the number of time-steps the
#'   landuse type in \code{from} must remain the same before it can change into
#'   the landuse type in \code{to}.
#' @param auto [`integerish(1)`][integer]\cr the number of time-steps after
#'   which the landuse type in \code{from} will automatically change into
#'   \code{to}.
#' @param mask [`character(1)`][character]\cr the name of a gridded object for
#'   which conversion to the specified class is allowed.
#' @details From the resulting land-use change sequence, a land-use conversion
#'   matrix is derived.
#' @family functions to specify landuse
#' @importFrom checkmate assertCharacter assertChoice assertNames assertNumeric
#' @export

setConversion <- function(from = NULL, to = NULL, label = NULL, thresh = NULL,
                          auto = NULL, mask = NULL){

  opts <- getOption("lclu")

  assertCharacter(x = from, any.missing = FALSE, len = 1)
  assertCharacter(x = to, any.missing = FALSE)
  targetSystems <- opts$attributes$landuse$system
  assertChoice(x = to, choices = targetSystems)
  assertNames(x = to, disjunct.from = from)
  assertChoice(x = from, choices = targetSystems)
  assertCharacter(x = label, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertNumeric(x = thresh, any.missing = FALSE, len = 1, null.ok = TRUE)
  assertCharacter(x = mask, any.missing = FALSE, null.ok = TRUE)
  if(sum(c(!is.null(auto), !is.null(thresh), !is.null(mask))) > 1){
    stop("only one of 'thresh', 'auto' or 'mask' can be set at a time.")
  }


  # set the conversion in allow.txt
  allow <- opts$tables$allow
  if(!is.null(thresh)){
    temp <- 100 + thresh
    if(is.null(label)){
      label <- ""
    } else {
      label <- paste0("\n(", label, ")")
    }
    label <- paste0("if older ", thresh, label)
  } else if(!is.null(auto)){
    temp <- 1000 + auto
    if(is.null(label)){
      label <- ""
    } else {
     label <- paste0("\n(", label, ")")
    }
    label <- paste0("auto change, ", auto, label)
  } else if(!is.null(mask)){
    temp <- which(opts$attributes$gridded$driver %in% mask) - 1
    if(temp == 1){
      stop("please don't register the gridded object '", mask, "' as the first or second object.")
    }
    opts$tables$main["value"][which(opts$tables$main$var == "drivers_max"),] <-
      as.character(as.numeric(opts$tables$main["value"][which(opts$tables$main$var == "drivers_max"),]) + 1)
    opts$tables$main["value"][which(opts$tables$main$var == "drivers_tot"),] <-
      as.character(as.numeric(opts$tables$main["value"][which(opts$tables$main$var == "drivers_tot"),]) + 1)
    if(is.null(label)) label <- ""
  } else {
    temp <- 1
    if(is.null(label)) label <- ""
  }

  # test whether a value has already been set
  if(allow[to][which(allow$system == from),] != 0){
    stop(paste0("the conversion from '", from, "' to '", to, "' has already been specified (", allow[to][which(allow$system == from),], ")"))
  }

  allow[to][which(allow$system == from),] <- temp
  opts$tables$allow <- allow

  type <- opts$tables$allowTypes
  type[to][which(type$system == from),] <- label
  opts$tables$allowTypes <- type

  options(lclu = opts)

  invisible(from)
}
