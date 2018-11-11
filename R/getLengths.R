#' @title Return variable lengths from the input schema.
#'
#' @description \code{getLengths} extracts the slot \code{df} of the input object and returns its
#' column \code{length} as a vector.
#'
#' @param object Object of class \linkS4class{StfwfSchema}.
#'
#' @return A character vector.
#'
#' @examples
#' # A trivial example:
#' df <- data.frame(variable = c('Turnover', 'Employees'),
#'                  length = c(9L, 3L),
#'                  initialPos = c(1, 10),
#'                  finalPos = c(9, 12),
#'                  type = rep('num', 2),
#'                  valueRegEx = c('[0-9]{0,9}', '[0-9]{0,3}'),
#'                  description = c('Turnover of the business unit',
#'                                  'Number of employees of the business unit'),
#'                  stringsAsFactors = FALSE)
#' Schema <- new(Class = 'StfwfSchema', df = df)
#' getLengths(Schema)
#'
#'
#' @export
setGeneric("getLengths", function(object){standardGeneric("getLengths")})

#' @rdname getLengths
#'
#' @export
setMethod(
  f = "getLengths",
  signature = c("StfwfSchema"),
  function(object){object@df[['length']]}
)

