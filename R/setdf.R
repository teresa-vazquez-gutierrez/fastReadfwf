#' @title Set slot \code{df}.
#'
#' @description \code{setdf} sets the slot \code{df}.
#'
#' @param object object with the fwf file schema.
#'
#' @param value new data.frame with the specification of the schema.
#'
#' @return object with slot \code{df} updated.
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
#' Schema <- new(Class = 'StfwfSchema')
#' setdf(Schema) <- df
#'
#' @rdname setdf
#'
#' @export
setGeneric("setdf<-", function(object, value){standardGeneric("setdf<-")})

#' @rdname setdf
#'
#' @export
setReplaceMethod(
  f = "setdf",
  signature = c("StfwfSchema", "data.frame"),
  function(object, value){

    object@df <- value
    return(object)
  }
)
