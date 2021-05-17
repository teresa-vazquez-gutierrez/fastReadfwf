#' @title Build an object of class \linkS4class{StfwfSchema} from a csv file.
#'
#' @description \code{CSVToSchema} is a constructor of the class 
#' \linkS4class{StfwfSchema}.
#'
#' This constructor reads a csv file containing partially or totally the schema 
#' of the fixed-width file to read. So far, only English names are supported. 
#' This file must contain the following columns:
#'
#' \itemize{
#'
#'    \item \code{variable}: the name of the variable.
#'    \item \code{width}: the number of positions which the values of each 
#'    variable occupies in the file.
#'    \item \code{initialPos}: initial position of the field which the values of
#'     this variable occupies in the file.
#'    \item \code{finalPos}: final position of the field which the values of 
#'    this variable occupies in the file.
#'    \item \code{type}: type of the variable. It must be either \code{log}, 
#'    \code{int},  \code{num} or \code{char}.
#'    \item \code{valueRegEx}: regular expression (\link{regex}) for the values 
#'    of each variable.
#'    \item \code{description}: textual description of the variable.
#'
#' }
#'
#' The file may have a header or not. In the latter case, the order of columns 
#' is assumed to be that of the list above. English only is supported so far.
#'
#' @param csvname Name of the csv file containing the schema.
#'
#' @param sep The separator between columns. Defaults to ';'.
#'
#' @param header Does the first data line contain column names? Defaults to 
#' \code{TRUE}.
#'
#' @param lang Character vector of length 1 indicating the language for the 
#' header in the csv file (English: en -- default). So far only English is 
#' supported.
#'
#' @param ... Extra arguments for \code{\link[data.table]{fread}}.
#'
#' @return Return an object of class \linkS4class{StfwfSchema}.
#'
#' @examples
#' path <- system.file('extdata', package = 'fastReadfwf')
#' csvToSchema(file.path(path, 'SchemaSNHS_microdataWeb.csv'), header = TRUE)
#'
#' @import data.table
#'
#' @importFrom methods new
#'
#' @export
StcsvToSchema <- function(csvname, sep = ';', header = TRUE, lang = 'en', ...){

  width <- initialPos <- finalPos <- valueRegEx <- description <- NULL

  stColNames <- c('variable', 'width', 'initialPos', 'finalPos', 'type', 'valueRegEx','description')
  csv <- fread(csvname, sep = sep, header = header, blank.lines.skip = TRUE, ...)

  if (header == FALSE) {

    if (lang == 'en') {

      warning('[fastReadfwf::csvToSchema] No header specified. Standard names assigned.')
      setnames(csv, stColNames)

    }

  }

  if (header == TRUE) {

    if (lang == 'en') {


      diffNames_1 <- setdiff(unique(names(csv)), stColNames)
      if (length(diffNames_1) > 0) {

        stop(paste0('[StfwfSchema:: csvToSchema] Wrong column names:\n',
                    paste0(diffNames_1, collapse = ', '), '.\n'))

      }

      diffNames_2 <- setdiff(stColNames, unique(colnames(csv)))
      if (length(diffNames_2) > 0) {

        stop(paste0('[StfwfSchema:: csvToSchema] Missing column names:\n',
                    paste0(diffNames_2, collapse = ', '), '.\n'))

      }
      setcolorder(csv, stColNames)
    }
  }

  if (lang == 'en') {

    n <- dim(csv)[1]
    csv[
      , description := as.character(description)][
      , valueRegEx := as.character(valueRegEx)]

    # No initialPos and no finalPos: only width specified
    if (all(!is.na(csv$width)) & all(is.na(csv$finalPos)) & all(is.na(csv$initialPos))) {

      csv[, initialPos := 1 + c(0, cumsum(width)[-n])]
      csv[, finalPos := initialPos + width - 1]
    }

    # Whitespaces to .*
    csv[is.na(valueRegEx) | valueRegEx == '', valueRegEx := '[.]*']

  }

  output <- new(Class = 'StfwfSchema', df = csv)
  return(output)
}
