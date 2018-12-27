#' @title Set initial position(s) of variable(s)
#'
#' @description \code{setinitialPos} assigns an integer value (initial positition) to the
#' variable(s) specified in the input parameters.
#'
#' @param schema Object of class \linkS4class{StfwfSchema}.
#'
#' @param initialPos integer vector with the new initial positions of the variables.
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
#' setinitialPos(Schema, c(2L), c('Turnover'))
#'
#' @rdname setinitialPos
#'
#' @include getdf.R getVariables.R setdf.R
#'
#' @export
setGeneric("setinitialPos", function(schema, initialPos, variables){standardGeneric("setinitialPos")})

#' @rdname setinitialPos
#'
#' @export
setMethod(
  f = "setinitialPos",
  signature = c("StfwfSchema", "integer"),
  function(schema, initialPos, variables){

    if (missing(variables)) variables <- getVariables(schema)
    if (length(initialPos) != length(variables)) stop('[setWidths] Parameters initial positions and variables must have the same length.')
    if (length(initialPos) > nrow(getdf(schema))) stop('[setWidths] Parameter initial positions must be at most', nrow(getdf(schema)), ' long.')

    indexVar <- which(getVariables(schema) %in% variables)
    df <- getdf(schema)
    df[indexVar, 'initialPos'] <- initialPos
    df$width <- c(diff(df$initialPos), df$width[length(df$width)])
    df$finalPos <- df$initialPos + df$width - 1
    warning('[fastReadfwf::setfinalPos] Widths and final positions have been recomputed according to new initial positions.\n')
    setdf(schema) <- df
    #    validObject(schema)
    return(schema)

  }
)
