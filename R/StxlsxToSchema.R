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
#' @param xlsxname Name of the xlsx file containing the schema.
#'
#' @param sheetname Name or index of the sheet of the xlsx file.
#'
#' @param header Does the first data line contain column names? Defaults to \code{TRUE}.
#'
#' @param lang Character vector of length 1 indicating the language for the header in the xlsx file
#' (English: en).
#'
#' @param ... Extra arguments for \code{\link[data.table]{fread}}.
#'
#' @return Return an object of class \linkS4class{StfwfSchema}.
#'
#' @examples
#' path <- system.file('extdata', package = 'fastReadfwf')
#' StxlsxToSchema(file.path(path, 'SchemaSNHS.xlsx'), 'stSchema')
#'
#' @import data.table
#'
#' @importFrom openxlsx read.xlsx
#'
#' @importFrom methods new
#'
#' @export
StxlsxToSchema <- function(xlsxname, sheetname, header = TRUE, lang = 'en', ...){

  stColNames <- c('variable', 'width', 'initialPos', 'finalPos', 'type', 'valueRegEx','description')
  xlsx <- read.xlsx(xlsxname, sheet = sheetname, colNames = header, skipEmptyCols = FALSE, ...)

  if (header == FALSE) {

    if (lang == 'en') {

      warning(paste0('[fastReadfwf::StxlsxToSchema] No header specified. Standard names assigned in the following order: ',
                     paste0(stColNames, collapse = ', '), '\n.'))
      colnames(xlsx) <- stColNames

    }

  }
  if (header == TRUE) {

    if (lang == 'en') {


      diffNames_1 <- setdiff(unique(colnames(xlsx)), stColNames)
      if (length(diffNames_1) > 0) {

        stop(paste0('[StfwfSchema:: StxlsxToSchema] Wrong column names:\n',
                    paste0(diffNames_1, collapse = ', '), '.\n'))

      }

      diffNames_2 <- setdiff(stColNames, unique(colnames(xlsx)))
      if (length(diffNames_2) > 0) {

        stop(paste0('[StfwfSchema:: StxlsxToSchema] Missing column names:\n',
                    paste0(diffNames_2, collapse = ', '), '.\n'))

      }
      xlsx <- xlsx[, stColNames]
    }
  }

  if (lang == 'en') {

    n <- dim(xlsx)[1]

    # Classes of columns width, initialPos, finalPos must be integer
    xlsx$width <- as.integer(xlsx$width)
    widthNAs <- is.na(xlsx$width)
    invalidWidths <- xlsx[widthNAs, 'variable']
    if (sum(widthNAs) != 0 & sum(widthNAs) != n) {

      stop(paste0('[fastReadfwf::StxlsxToSchema] The following variables have wrong width: ',
                  paste0(invalidWidths, collapse = ', '), '.\n'))
    }

    xlsx$initialPos <- as.integer(xlsx$initialPos)
    initialPosNAs <- is.na(xlsx$initialPos)
    invalidinitialPos <- xlsx[initialPosNAs, 'variable']
    if (sum(initialPosNAs) != 0 & sum(initialPosNAs) != n) {

      stop(paste0('[fastReadfwf::StxlsxToSchema] The following variables have wrong initial positions: ',
                  paste0(invalidinitialPos, collapse = ', '), '.\n'))
    }

    xlsx$finalPos <- as.integer(xlsx$finalPos)
    finalPosNAs <- is.na(xlsx$finalPos)
    invalidfinalPos <- xlsx[finalPosNAs, 'variable']
    if (sum(finalPosNAs) != 0 & sum(finalPosNAs) != n) {

      stop(paste0('[fastReadfwf::StxlsxToSchema] The following variables have wrong final positions: ',
                  paste0(invalidfinalPos, collapse = ', '), '.\n'))
    }

    # No initialPos and no finalPos: only width specified
    if (all(!is.na(xlsx$width)) & all(is.na(xlsx$finalPos)) & all(is.na(xlsx$initialPos))) {

      xlsx$initialPos <- 1 + c(0, cumsum(xlsx$width)[-n])
      xlsx$finalPos <- xlsx$initialPos + xlsx$width - 1
    }

    # Whitespaces to .*
    xlsx$valueRegEx[is.na(xlsx$valueRegEx) | xlsx$valueRegEx == ''] <- '.*'

  }

  output <- new(Class = 'StfwfSchema', df = xlsx)
  return(output)

}
