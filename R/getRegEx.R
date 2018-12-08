#' @title Return regex for variable values from the input schema.
#'
#' @description \code{getRegEx} extracts the slot \code{df} of the input object and returns its
#' column \code{valueRegEx} as a vector.
#'
#' @param object Object of class \linkS4class{StfwfSchema}.
#'
#' @return A character vector with regular expressions for the values of each variable.
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
#' getRegEx(Schema)
#'
#' @include getdf.R getVariables.R
#'
#' @export
setGeneric("getRegEx", function(object){standardGeneric("getRegEx")})

#' @rdname getRegEx
#'
#' @export
setMethod(
  f = "getRegEx",
  signature = c("StfwfSchema"),
  function(object){

    out <- getdf(object)[['valueRegEx']]
    names(out) <- getVariables(object)
    return(out)

  }
)

