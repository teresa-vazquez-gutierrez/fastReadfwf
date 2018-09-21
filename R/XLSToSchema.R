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
#'    \item \code{length} (en) or \code{longitud} (sp): the number of positions which the values of
#'    this variable occupies in the file.
#'    \item \code{initialPos} (en) or \code{posInicial} (sp): initial position of the field which
#'    the values of this variable occupies in the file.
#'    \item \code{finalPos} (en) or \code{posFinal} (sp): final position of the field which the
#'    values of this variable occupies in the file.
#'    \item \code{type} (en) or \code{tipo} (sp): type of the variable. It must be either \code{num}
#'     or \code{char}.
#'    \item \code{valueRegEx} (en) or \code{regExValor} (sp): regular expression for the values of
#'    this variable.
#'    \item \code{description} (en) or \code{descripción} (sp): textual description of the variable.
#'
#' }
#'
#' The tag must have a header in file 1. English or Spanish (default) are supported.
#'
#' @param xlsname Name of the xls file containing the schema.
#'
#' @param lang Character vector of length 1 indicating the language for the header in the xls file
#' (English: en; Spanish: sp).
#'
#' @return Returns an object of class \linkS4class{StfwfSchema}.
#'
#' @examples
#' \dontrun{
#' XLSToSchema('schemaFile.xlsx')
#' }
#'
#' @import data.table
#'
#' @importFrom openxlsx read.xlsx
#'
#' @export
XLSToSchema <- function(xlsname, lang = 'sp'){

  if (lang == 'sp') tagName <- 'Schema'
  if (lang == 'en') tagName <- 'Schema'
  stColNames <- c('variable', 'length', 'initialPos', 'finalPos', 'type', 'valueRegEx',
                  'description')
  stColNames_sp <- c('variable', 'longitud', 'posInicial', 'posFinal',
                     'tipo', 'regExValor', 'descripción')
  Encoding(stColNames_sp) <- "UTF-8"

  xls <- openxlsx::read.xlsx(xlsname, sheet = tagName)

  if (lang == 'sp' && any(colnames(xls) != stColNames_sp)) {

    stop('[StfwfSchema:: XLSToSchema] Column names not valid. Check the documentation for correct names.\n')

  }

  if (lang == 'en' && any(colnames(xls) != stColNames)) {

    stop('[StfwfSchema:: XLSToSchema] Column names not valid. Check the documentation for correct names.\n')

  }

  if (lang == 'sp') {

    colnames(xls) <- c('variable', 'length', 'initialPos',
                       'finalPos', 'type', 'valueRegEx', 'description')

  }

  if (all(is.na(xls$finalPos)) & all(is.na(xls$initialPos))) {

    xls$initialPos <- 1 + c(0, cumsum(xls$length)[-dim(xls)[1]])
    xls$finalPos <- xls$initialPos + xls$length - 1

  }

  if (all(is.na(xls$valueRegEx))) {

    xls$valueRegEx <- '[.]+'

  }

  output <- new(Class = 'StfwfSchema', df = xls)
  return(output)

}
