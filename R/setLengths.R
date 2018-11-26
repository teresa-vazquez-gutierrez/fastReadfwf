#' @title Set length(s) of variable(s)
#'
#' @description \code{setLengths} assigns an integer value (length) to the variable(s) specified in
#' the input parameters.
#'
#' @param schema Object of class \linkS4class{StfwfSchema}.
#'
#' @param lengths integer vector with the new lengths of the variables.
#'
#' @param variables character vector with the names of the variables whose lengths are to be
#' updated.
#'
#' @return Object of class \linkS4class{StfwfSchema}.
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
#' setLengths(Schema, 10, 'Turnover')
#' setLengths(Schema, c(10L, 4L))
#'
#' @rdname setLengths
#'
#' @include getdf.R getVariables.R setdf.R
#'
#' @export
setGeneric("setLengths", function(schema, lengths, variables){standardGeneric("setLengths")})

#' @rdname setLengths
#'
#' @export
setMethod(
  f = "setLengths",
  signature = c("StfwfSchema", "integer"),
  function(schema, lengths, variables){

    if (missing(variables)) variables <- getVariables(schema)
    if (length(lengths) != length(lengths)) stop('[setLengths] Parameters lengths and variables must have the same length.')
    if (length(lengths) > nrow(getdf(schema))) stop('[setLengths] Parameter lengths must be at most', nrow(getdf(schema)), ' long.')


    indexVar <- which(getVariables(schema) %in% variables)
    df <- getdf(schema)
    df[indexVar, 'length'] <- lengths
    setdf(schema) <- df
    validObject(schema)
    return(schema)

  }
)
