#' CLUE scene class (S4) and methods
#'
#' A \code{scene} stores metadata and configuration of a CLUE model.
#'
#' @slot meta [`list(3)`][list]\cr
#' @slot period [`numeric(2)`][numeric]\cr
#' @slot landtypes [`list(.)`][list]\cr
#' @slot drivers [`list(.)`][list]\cr
#' @slot commodities [`list(.)`][list]\cr
#' @slot landsystems [`data.frame(.)`][data.frame]\cr
#' @slot validated [`logical(1)`][logical]\cr
#' @details

scene <- setClass(Class = "scene",
                  slots = c(meta = "list",
                            period = "numeric",
                            landsystems = "data.frame",
                            drivers = "list",
                            landtypes = "list",
                            commodities = "list")
)

setValidity("scene", function(object){

  errors = character()

  if (!.hasSlot(object = object, name = "meta")) {
    errors = c(errors, "the scene does not have a slot named 'meta'.")
  } else {
    if (!is.list(object@meta)) {
      errors = c(errors, "the slot 'meta' is not a list.")
    }
    if(!all(names(object@meta) %in% c("name", "version", "path", "description"))){
      errors <- c(errors, "'names(schema$meta)' must be a permutation of set {name,version,description}")
    }
    if(!is.null(object@meta$name)){
      if(!is.character(object@meta$name)){
        errors <- c(errors, "'schema$meta$name' must have a character value.")
      }
    }
    if(!is.null(object@meta$version)){
      if(!is.character(object@meta$version)){
        errors <- c(errors, "'schema$meta$version' must have a character value.")
      }
    }
    if(!is.null(object@meta$path)){
      if(!is.character(object@meta$path)){
        errors <- c(errors, "'schema$meta$path' must have a character value.")
      }
    }
    if(!is.null(object@meta$description)){
      if(!is.character(object@meta$description)){
        errors <- c(errors, "'schema$meta$description' must have a character value.")
      }
    }
  }

  if (!.hasSlot(object = object, name = "period")) {
    errors = c(errors, "the scene does not have a slot named 'period'.")
  } else {
    if(!is.null(object@period)){
      if(!is.numeric(object@period)){
        errors <- c(errors, "'schema$period' must have a numeric value.")
      }
    }
  }

  if (!.hasSlot(object = object, name = "landtypes")) {
    errors = c(errors, "the scene does not have a slot named 'landtypes'.")
  } else {
    if(!is.null(object@landtypes)){
      if(!is.list(object@landtypes)){
        errors <- c(errors, "'schema$landtypes' must be a list..")
      }
    }
  }

  if (!.hasSlot(object = object, name = "commodities")) {
    errors = c(errors, "the scene does not have a slot named 'commodities'.")
  } else {
    if(!is.null(object@commodities)){
      if(!is.list(object@commodities)){
        errors <- c(errors, "'schema$commodities' must be a list.")
      }
    }
  }

  if (!.hasSlot(object = object, name = "landsystems")) {
    errors = c(errors, "the scene does not have a slot named 'landsystems'.")
  } else {
    if(!is.null(object@landsystems)){
      if(!is.data.frame(object@landsystems)){
        errors <- c(errors, "'schema$landsystems' must be a data.frame.")
      }
    }
  }

  if (!.hasSlot(object = object, name = "drivers")) {
    errors = c(errors, "the scene does not have a slot named 'drivers'.")
  } else {
    if(!is.null(object@drivers)){
      if(!is.list(object@drivers)){
        errors <- c(errors, "'schema$drivers' must be a list.")
      }
    }
  }

  if(length(errors) == 0){
    return(TRUE)
  } else {
    return(errors)
  }

})

#' Print scene in the console
#'
#' @param object [`scene(1)`][scene]\cr object to \code{show}.
#' @details
#' @importFrom utils head
#' @importFrom crayon yellow red cyan
#' @importFrom purrr map
#' @importFrom stringr str_split
#' @importFrom dplyr last
#' @importFrom stats na.omit

setMethod(f = "show",
          signature = "scene",
          definition = function(object){

            message("please use 'str(scene)' for now")

          }

)
