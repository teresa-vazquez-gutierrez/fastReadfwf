#' @title Set final position(s) of variable(s)
#'
#' @description \code{setfinalPos} assigns an integer value (final positition) to the variable(s)
#' specified in the input parameters.
#'
#' @param schema Object of class \linkS4class{StfwfSchema}.
#'
#' @param finalPos integer vector with the new final positions of the variables.
#'
#' @param variables character vector with the names of the variables whose final positions are to be
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
#' setfinalPos(Schema, c(8L), c('Turnover'))
#'
#' @rdname setfinalPos
#'
#' @include getdf.R getVariables.R setdf.R getinitialPos.R getfinalPos.R
#'
#' @export
setGeneric("setfinalPos", function(schema, finalPos, variables){standardGeneric("setfinalPos")})

#' @rdname setfinalPos
#'
#' @export
setMethod(
  f = "setfinalPos",
  signature = c("StfwfSchema", "integer"),
  function(schema, finalPos, variables){

    if (missing(variables)) variables <- getVariables(schema)
    if (length(finalPos) != length(variables)) stop('[setWidths] Parameters final positions and variables must have the same length.')
    if (length(finalPos) > nrow(getdf(schema))) stop('[setWidths] Parameter final positions must be at most', nrow(getdf(schema)), ' long.')

    indexVar <- which(getVariables(schema) %in% variables)
    df <- getdf(schema)
    df[indexVar, 'finalPos'] <- finalPos
    df$width <- df$finalPos - df$initialPos + 1
    warning('[fastReadfwf::setfinalPos] Widths have been recomputed according to initial positions and new final positions.\n')
    setdf(schema) <- df
#    validObject(schema)
    return(schema)

  }
)
