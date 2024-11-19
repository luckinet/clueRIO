#' Set options of the current model run
#'
#' @param iter_mode [`integerish(1)`][integer]\cr Iteration mode: \itemize{\item
#'   0 = convergence criteria are expressed as a percentage of the demand, \item
#'   1 = convergence criteria are expressed as absolute values, \item 2 = not
#'   yet implemented}
#' @param conv1 [`numeric(1)`][numeric]\cr First convergence criterion.
#' @param conv2 [`numeric(1)`][numeric]\cr Second convergence criterion.
#' @param out_type [`integerish(1)`][integer]\cr Choice for the type of output
#'   file:
#'   \itemize{ \item 0 = no headers in output file, \item 1 = ArcGIS headers,
#'   \item -2 = no headers and suppress information in log-file, \item 2 =
#'   ArcGIS headers and suppress information in log-file, \item 3 =
#'   ArcGIS/ArcMap extension and headers (default)}
#' @param rs_reg [`integerish(1)`][integer]\cr Region specific regressions.
#'   \itemize{ \item 0 = no different regressions for different regions
#'   (default), \item 1 = different regressions for different regions, \item 2
#'   = different regressions for different regions.}
#' @param ls_hist [`integerish(2)`][integer]\cr Initialization of land use
#'   history:
#'   \itemize{ \item 0 = read from file age, \item 1 = generate random number +
#'   standard seed (default), \item 2 = merely random number} For options 1 and
#'   2, an additional value for the maximum number of years of the random
#'   numbers must be given.
#' @param neigh [`integerish(1)`][integer]\cr Neighbourhood function (not yet
#'   implemented): \itemize{ \item 0 = off (default), \item 1 = on in simulation
#'   (automatically chosen on use of \code{\link{setNeighbourhood}}), \item 2 =
#'   only influences calculated, no simulation}
#' @param loc_pref [`integerish(1)`][integer]\cr
#' @param dyn_lusmat [`integerish(1)`][integer]\cr Dynamic landuse matrix (not
#'   yet implemented): \itemize{ \item 0 = off (default), \item 1 = on}
#' @param out_write [`integerish(1)`][integer]\cr Write selected output
#'   measures:
#'   \itemize{ \item 0 = off, \item 1 = on for final year (default), \item 2 =
#'   on for all years}
#' @param iter_param [`numeric(1)`][numeric]\cr Choice for stability vs. speed
#'   of convergence. Value should be between 0.001 and 0.1, with higher values
#'   leading to more stable and more likely solution and lower values leading to
#'   faster and less likely solution.
#' @param protected [`logical(1)`][logical]\cr whether or not to consider
#'   protected areas (if the gridded object 'protected' has been given). (not
#'   implemented yet)
#' @return no return value, called for the side-effect of setting model options.
#' @importFrom checkmate assertIntegerish assertNumeric
#' @importFrom stringr str_split
#' @export

clue_options <- function(iter_mode = NULL, conv1 = NULL, conv2 = NULL,
                         out_type = NULL, rs_reg = NULL, ls_hist = NULL,
                         neigh = NULL, loc_pref = NULL, dyn_lusmat = NULL,
                         out_write = NULL, iter_param = NULL, protected = TRUE){

  assertIntegerish(x = iter_mode, lower = 0, upper = 2, null.ok = TRUE)
  assertNumeric(x = conv1, lower = 0, upper = 100, null.ok = TRUE)
  assertNumeric(x = conv2, lower = 0, upper = 100, null.ok = TRUE)
  assertIntegerish(x = out_type, lower = -2, upper = 3, null.ok = TRUE)
  assertIntegerish(x = rs_reg, lower = 0, upper = 2, null.ok = TRUE)
  assertIntegerish(x = ls_hist, max.len = 2, null.ok = TRUE)
  assertIntegerish(x = neigh, null.ok = TRUE)
  assertIntegerish(x = loc_pref, lower = 0, upper = 1, null.ok = TRUE)
  assertIntegerish(x = dyn_lusmat, lower = 0, upper = 1, null.ok = TRUE)
  assertIntegerish(x = out_write, lower = 0, upper = 2, null.ok = TRUE)
  assertNumeric(x = iter_param, null.ok = TRUE)

  opts <- getOption("clue")

  params <- opts$options

  iter_vars <- str_split(params[["value"]][14], pattern = "\t")[[1]]
  if(!is.null(iter_mode)){
    iter_vars[1] <- iter_mode
  }
  if(!is.null(conv1)){
    if(conv1 > 0.5 | conv1 < 0.05){
      warning("the first convergence criterion (conv1) should typically be between 0.05 and 0.5, but is ", conv1)
    }
    iter_vars[2] <- conv1
  }
  if(!is.null(conv2)){
    if(conv2 > 1 | conv2 < 0.1){
      warning("the second convergence criterion (conv2) should typically be between 0.1 and 1, but is ", conv2)
    }
    iter_vars[3] <- conv2
  }
  params[["value"]][14] <- paste0(iter_vars, collapse = "\t")

  if(!is.null(out_type)){
    if(out_type == -1){
      out_type <- -2
    }
    params[["value"]][17] <- out_type
  }

  if(!is.null(rs_reg)){
    params[["value"]][18] <- rs_reg
  }

  if(!is.null(ls_hist)){
    assertIntegerish(x = ls_hist[1], lower = 0, upper = 2)
    if(ls_hist[1] != 0){
      assertIntegerish(x = ls_hist, len = 2)
    }
    params[["value"]][19] <- paste(ls_hist, collapse = "\t")
  }

  if(!is.null(neigh)){
    params[["value"]][20] <- neigh
  }

  if(!is.null(dyn_lusmat)){
    # params[["value"]][22] <- dyn_lusmat
    message("'dynamic landuse matrix' has not yet been implemented.")
  }

  if(!is.null(out_write)){
    params[["value"]][23] <- out_write
  }

  if(!is.null(iter_param)){
    if(iter_param > 0.1 | iter_param < 0.001){
      warning("the iteration parameter (iter_param) should typically be between 0.001 and 0.1, but is ", iter_param)
    }
    params[["value"]][24] <- iter_param
  }

  opts$options <- params

  # store the root path in the options
  oldOptions <- options()
  on.exit(options(oldOptions))

  options(clue = opts)

}
