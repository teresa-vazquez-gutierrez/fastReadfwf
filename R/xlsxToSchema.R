#' @title Build an object of class \linkS4class{StfwfSchema}.
#'
#' @description \code{XLSToSchema} is a constructor of the class \linkS4class{StfwfSchema}.
#'
#' This constructor reads an Excel file containing partially or totally the schema of the
#' fixed-width file to read. This file must contain a tag with name \code{Schema} (en) or
#' \code{DiseñoRegistro} (sp) and the following named columns:
#'
#' \itemize{
#'
#'    \item \code{variable} (en, sp): the name of the variable.
#'    \item \code{width} (en) or \code{anchura} (sp): the number of positions which the values of
#'    this variable occupies in the file.
#'    \item \code{initialPos} (en) or \code{posInicial} (sp): initial position of the field which
#'    the values of this variable occupies in the file.
#'    \item \code{finalPos} (en) or \code{posFinal} (sp): final position of the field which the
#'    values of this variable occupies in the file.
#'    \item \code{type} (en) or \code{tipo} (sp): type of the variable. It must be either \code{log},
#'    \code{integer}, \code{num} or \code{char}.
#'    \item \code{valueRegEx} (en) or \code{regExValor} (sp): regular expression for the values of
#'    this variable.
#'    \item \code{description}: textual description of the variable.
#'
#' }
#'
#' The tag must have a header in file 1. Only English is supported so far.
#'
#' @param xlsname Name of the xlsx file containing the schema.
#'
#' @param sheetname Name or index of the sheet of the xlsx file.
#'
#' @param lang Character vector of length 1 indicating the language for the header in the xlsx file
#' (English: en).
#'
#' @return Returns an object of class \linkS4class{StfwfSchema}.
#'
#' @examples
#' path <- system.file('extdata', package = 'fastReadfwf')
#' xlsxToSchema(file.path(path, 'Schema.SNHS.xlsx'), 'stENSE2017Adulto_Schema')
#'
#' @import data.table
#'
#' @importFrom openxlsx read.xlsx
#'
#' @importFrom methods new
#'
#' @export
xlsxToSchema <- function(xlsname, sheetname, lang = 'en'){

  stColNames <- c('variable', 'width', 'initialPos', 'finalPos', 'type', 'valueRegEx','description')

  xlsx <- read.xlsx(xlsname, sheet = sheetname)

  if (lang == 'en' && any(colnames(xlsx) != stColNames)) {

    diffNames <- paste0(colnames(xlsx)[colnames(xlsx) != stColNames], collapse = ', ')
    stop(
      paste0('[StfwfSchema:: XLSToSchema] The following columns have invalid names: ',
             diffNames,
             '.\n')
    )
  }

  if (all(is.na(xlsx$finalPos)) & all(is.na(xlsx$initialPos))) {

    xlsx$initialPos <- 1 + c(0, cumsum(xlsx$width)[-dim(xlsx)[1]])
    xlsx$finalPos <- xlsx$initialPos + xlsx$width - 1

  }

  xlsx$valueRegEx[is.na(xlsx$valueRegEx)] <- '.*'
  output <- new(Class = 'StfwfSchema', df = xlsx)
  return(output)

}
