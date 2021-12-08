#' @title Extract parts of an object of class \linkS4class{StfwfSchema}
#'
#' @description \code{[} extracts parts of an object of class \linkS4class{StfwfSchema}.
#'
#' It is indeed the method \code{[} for the class \linkS4class{StfwfSchema}. This method returns
#' subsets of the slot \code{df} from an object of class \linkS4class{StfwfSchema} specified as an
#' input parameter. The output is an object of the same class \linkS4class{StfwfSchema} as the input
#'  parameter \code{x}.
#'
#' @param x Object of class \linkS4class{StfwfSchema}.
#'
#' @param i,j,... Indices corresponding to elements to be extracted. The indices are numeric or
#' character vectors, \code{\link{missing}} or \code{\link{NULL}}. Numeric values are coerced to
#' \code{integer} with \code{\link{as.integer}} (thus truncated to zero).
#'
#' @param drop Included by coherence.
#'
#' @return Object of class \linkS4class{StfwfSchema} with the subsetted input object.
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
#' Schema[variable == 'Turnover', ]
#' Schema[1, 'valueRegEx']
#'
#' @include subStfwfSchema.R getdf.R setdf.R
#'
#' @export
setMethod(
  f = "[",
  signature = c("StfwfSchema"),
  function(x, i, j, ..., drop=TRUE){
    
    mc <- match.call()
    auxDF <- getdf(x)
    mc[[1L]] <- `[.data.frame`
    mc[['x']] <- auxDF
    x.subsetted <- eval(mc, envir = auxDF, enclos = parent.frame())
    return(x.subsetted)
})
