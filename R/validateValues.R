#' @title Fast read a fixed-width file
#'
#' @description \code{fread_fwf} takes as basic input a fixed-width filename and its schema and
#' wraps around \code{\link[data.table]{fread}} from package \linkS4class{data.table} to provide the
#' contents of the file.
#'
#' This method is indeed a wrapper for the function \code{\link[data.table]{fread}} followed by the
#' application of \code{\link[stringi]{stri_sub}}.
#'
#' @param object Object with rectangular shape whose columns will be validated.
#'
#' @param StfwfSchema Object of class \linkS4class{StfwfSchema} with the schema of object.
#'
#' @param perl Logical vector of length 1 with default value \code{FALSE} to indicate whether to use
#' perl or not in the application of regexp.
#'
#' @return Returns \code{TRUE}.
#'
#' @examples
#' path <- system.file('extdata', package = 'fastReadfwf')
#' stSchema <- fastReadfwf::StxlsxToSchema(
#'    file.path(path, 'SchemaSNHS.xlsx'),
#'    sheetname = 'stSchema')
#'
#' # For data.tables
#' data <- fread_fwf(
#' file.path(path, 'MicroDataSNHS.txt'), stSchema, outFormat = 'data.table', perl = TRUE)
#' validateValues(data, stSchema, perl = TRUE)
#'
#' # For tibbles
#' data <- fread_fwf(
#' file.path(path, 'MicroDataSNHS.txt'), stSchema, outFormat = 'tibble')
#' validateValues(data, stSchema, perl = TRUE)
#'
#'
#' @import data.table
#'
#' @include StfwfSchema-class.R getVariables.R getRegEx.R
#'
#' @export
setGeneric("validateValues",
           function(object, StfwfSchema, perl = FALSE) {standardGeneric("validateValues")})

#' @rdname validateValues
#'
#' @export
setMethod(f = "validateValues",
          signature = c("data.frame", "StfwfSchema"),
          function(object, StfwfSchema, perl = FALSE){

  cat('[fastReadfwf:: validateValues] Value patterns will be checked for each variable.\n\n')
  varNames <- getVariables(StfwfSchema)
  valueRegEx <- getRegEx(StfwfSchema)
  lapply(seq(along = varNames), function(i){

    cat(paste0('Checking variable ', varNames[i], '... '))
    pattern <- valueRegEx[i]
    values <- object[[varNames[i]]]

    wrongValuesindex <- which(regexpr(pattern, values, perl = perl) == -1)
    if (length(wrongValuesindex) == 0) {

      cat('ok.\n')

    } else {

      stop(paste0('\n Please revise either the data set or the regex for this variable.\n\n The following values do not follow the pattern:\n',
                              paste0(object[[varNames[i]]][wrongValuesindex], collapse = ', ')))
    }
  })
  return(TRUE)
})
