#' @title Set width(s) of variable(s)
#'
#' @description \code{setWidths} assigns an integer value (width) to the variable(s) specified in
#' the input parameters.
#'
#' @param schema Object of class \linkS4class{StfwfSchema}.
#'
#' @param widths integer vector with the new widths of the variables.
#'
#' @param variables character vector with the names of the variables whose widths are to be
#' updated.
#'
#' @return Object of class \linkS4class{StfwfSchema}.
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
#' setWidths(Schema, c(6L), c('Turnover'))
#'
#' @rdname setWidths
#'
#' @include getdf.R getVariables.R setdf.R getinitialPos.R getfinalPos.R
#'
#' @importFrom methods validObject
#'
#' @export
setGeneric("setWidths", function(schema, widths, variables){standardGeneric("setWidths")})

#' @rdname setWidths
#'
#' @export
setMethod(
  f = "setWidths",
  signature = c("StfwfSchema", "integer"),
  function(schema, widths, variables){

    if (missing(variables)) variables <- getVariables(schema)
    if (length(widths) != length(variables)) stop('[setWidths] Parameters widths and variables must have the same length.')
    if (length(widths) > nrow(getdf(schema))) stop('[setWidths] Parameter widths must be at most', nrow(getdf(schema)), ' long.')

    indexVar <- which(getVariables(schema) %in% variables)
    df <- getdf(schema)
    df[indexVar, 'width'] <- widths
    df$finalPos <- df$initialPos + df$width - 1
    warning('[fastReadfwf::setWidths] Final positions have been recomputed according to initial positions and new widths.\n')
    setdf(schema) <- df
    validObject(schema)
    return(schema)

  }
)
