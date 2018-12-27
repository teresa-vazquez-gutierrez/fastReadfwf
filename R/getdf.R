#' @title Get slot \code{df} from the input object.
#'
#' @description \code{getdf} extracts the slot \code{df} of the input object.
#'
#' @param object Object of class \linkS4class{StfwfSchema}.
#'
#' @return A data.frame.
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
#' getdf(Schema)
#'
#'
#' @export
setGeneric("getdf", function(object){standardGeneric("getdf")})

#' @rdname getdf
#'
#' @export
setMethod(
  f = "getdf",
  signature = c("StfwfSchema"),
  function(object){object@df}
)

