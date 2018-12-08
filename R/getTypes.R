#' @title Return variable types from the input schema.
#'
#' @description \code{getTypes} extracts the slot \code{df} of the input object and returns its
#' column \code{type} as a vector.
#'
#' @param object Object of class \linkS4class{StfwfSchema}.
#'
#' @return A character vector with component entries \code{'num'}, \code{'char'}, and/or
#' \code{'bool'}).
#'
#' @examples
#' # A trivial example:
#' df <- data.frame(variable = c('Turnover', 'Employees'),
#'                  width = c(9L, 3L),
#'                  initialPos = c(1, 10),
#'                  finalPos = c(9, 12),
#'                  type = rep('num', 2),
#'                  valueRegEx = c('[0-9]{0,9}', '[0-9]{0,3}'),
#'                  description = c('Turnover of the business unit',
#'                                  'Number of employees of the business unit'),
#'                  stringsAsFactors = FALSE)
#' Schema <- new(Class = 'StfwfSchema', df = df)
#' getTypes(Schema)
#'
#' @include getdf.R getVariables.R
#'
#' @export
setGeneric("getTypes", function(object){standardGeneric("getTypes")})

#' @rdname getTypes
#'
#' @export
setMethod(
  f = "getTypes",
  signature = c("StfwfSchema"),
  function(object){

    out <- getdf(object)[['type']]
    names(out) <- getVariables(object)
    return(out)

  }
)

