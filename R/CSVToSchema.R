#' @title Build an object of class \linkS4class{StfwfSchema}.
#'
#' @description \code{CSVToSchema} is a constructor of the class \linkS4class{StfwfSchema}.
#'
#' This constructor reads a csv file containing partially or totally the schema of the fixed-width
#' file to read. So far, only English names are supported. This file must contain the following
#' columns:
#'
#' \itemize{
#'
#'    \item \code{variable} (en): the name of the variable.
#'    \item \code{width} (en): the number of positions which the values of each variable occupies in
#'     the file.
#'    \item \code{initialPos} (en): initial position of the field which the values of this variable
#'    occupies in the file.
#'    \item \code{finalPos} (en): final position of the field which the values of this variable
#'    occupies in the file.
#'    \item \code{type} (en): type of the variable. It must be either \code{log}, \code{integer},
#'    \code{num} or \code{char}.
#'    \item \code{valueRegEx} (en): regular expression for the values of each variable.
#'    \item \code{description}: textual description of the variable.
#'
#' }
#'
#' The file may have a header or not. In the latter case, the order of columns is assumed to be that
#'  of the list above. English only is supported so far.
#'
#' @param csvname Name of the csv file containing the schema.
#'
#' @param sep The separator between columns. Defaults to ';'.
#'
#' @param header Does the first data line contain column names? Defaults to \code{FALSE}.
#'
#' @param lang Character vector of length 1 indicating the language for the header in the csv file
#' (English: en -- default). So far only English is supported.
#'
#' @param ... Extra arguments for \code{\link[data.table]{fread}}.
#'
#' @return Return an object of class \linkS4class{StfwfSchema}.
#'
#' @examples
#' path <- system.file('extdata', package = 'fastReadfwf')
#' csvToSchema(file.path(path, 'Schema.SNHS.csv'), header = TRUE)
#'
#' @import data.table
#'
#' @importFrom methods new
#'
#' @export
csvToSchema <- function(csvname, sep = ';', header = FALSE, lang = 'en', ...){

  stColNames <- c('variable', 'width', 'initialPos', 'finalPos',
                  'type', 'valueRegEx', 'description')

  if (!header) {

    csv <- fread(csvname, sep = sep, header = header, blank.lines.skip = TRUE, ...)
    if (lang == 'en') setnames(csv, stColNames)
  }

  if (header) csv <- fread(csvname, sep = sep, header = header, blank.lines.skip = TRUE, ...)

  csv[, (names(csv)[7]) := as.character(get(names(csv)[7]))][
    , (names(csv)[6]) := as.character(get((names(csv)[6])))]
  if (lang == 'en') {

    setcolorder(csv, stColNames)
    csv$valueRegEx[is.na(csv$valueRegEx)] <- '.*'

  }
  output <- new(Class = 'StfwfSchema', df = csv)
  return(output)
}
