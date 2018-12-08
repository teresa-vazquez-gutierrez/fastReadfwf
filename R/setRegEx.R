#' @title Set regex for the accepted values of the variable(s)
#'
#' @description \code{setRegEx} assigns a regexp to the values of each variable specified in
#' the input parameters.
#'
#' @param schema Object of class \linkS4class{StfwfSchema}.
#'
#' @param regex charactec vector with the new regex for the accepted values of the variables.
#'
#' @param variables character vector with the names of the variables whose regex are to be
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
#' setRegEx(Schema, c('[0-9]{0,10}'), 'Turnover')
#'
#' @rdname setRegEx
#'
#' @include getdf.R getVariables.R setdf.R
#'
#' @export
setGeneric("setRegEx", function(schema, regex, variables){standardGeneric("setRegEx")})

#' @rdname setRegEx
#'
#' @export
setMethod(
  f = "setRegEx",
  signature = c("StfwfSchema", "character"),
  function(schema, regex, variables){

    if (missing(variables)) variables <- getVariables(schema)
    if (length(regex) != length(variables)) stop('[setWidths] Parameters widths and variables must have the same length.')
    if (length(regex) > nrow(getdf(schema))) stop('[setWidths] Number of regex must be at most', nrow(getdf(schema)), '.')


    indexVar <- which(getVariables(schema) %in% variables)
    df <- getdf(schema)
    df[indexVar, 'valueRegEx'] <- regex
    setdf(schema) <- df
    validObject(schema)
    return(schema)

  }
)
