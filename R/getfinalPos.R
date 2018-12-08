#' @title Return variable final positions from the input schema.
#'
#' @description \code{getfinalPos} extracts the slot \code{df} of the input object and returns its
#' column \code{finalPos} as a vector.
#'
#' @param object Object of class \linkS4class{StfwfSchema}.
#'
#' @return An integer vector.
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
#' getfinalPos(Schema)
#'
#' @include getdf.R getVariables.R
#'
#' @export
setGeneric("getfinalPos", function(object){standardGeneric("getfinalPos")})

#' @rdname getfinalPos
#'
#' @export
setMethod(
  f = "getfinalPos",
  signature = c("StfwfSchema"),
  function(object){

    out <- getdf(object)[['finalPos']]
    names(out) <- getVariables(object)
    return(out)

  }
)

