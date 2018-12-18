#' @title Build an object of class \linkS4class{StfwfSchema}.
#'
#' @description \code{CSVToSchema} is a constructor of the class
#' \linkS4class{StfwfSchema}.
#'
#' This constructor reads a csv file containing partially or totally the schema
#' of the fixed-width file to read. This file must contain the following
#' columns:
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
#'    \item \code{description} (en) or \code{descripción} (sp): textual description of the variable.
#'
#' }
#'
#' The file may have a header or not. In the latter case, the order of columns
#' is assumed to be that of the list above. English or Spanish (default) are
#' supported.
#'
#' @param csvname Name of the csv file containing the schema.
#'
#' @param sep The separator between columns. Defaults to ';'.
#'
#' @param header Does the first data line contain column names? Defaults to
#' \code{FALSE}.
#'
#' @param lang Character vector of length 1 indicating the language for the
#' header in the csv file (English: en; Spanish: sp).
#'
#' @return Returns an object of class \linkS4class{StfwfSchema}.
#'
#' @examples
#' \dontrun{
#' CSVToSchema('C:/E30103.MonitorizacionFL.SchemaIRIA.csv')
#' }
#'
#' @import data.table
#'
#' @importFrom methods new
#'
#' @export
CSVToSchema <- function(csvname, sep = ';', header = FALSE, lang = 'en'){

  stColNames <- c('variable', 'width', 'initialPos', 'finalPos',
                  'type', 'valueRegEx', 'description')
  stColNames_sp <- c('variable', 'anchura', 'posInicial', 'posFinal',
                     'tipo', 'regExValor', 'descripción')
  Encoding(stColNames_sp) <- "UTF-8"

  if (!header) {

    csv <- data.table::fread(csvname, sep = sep, header = header)
    if (lang == 'sp') setnames(csv, stColNames_sp)
    if (lang == 'en') setnames(csv, stColNames)
  }

  if (header) csv <- data.table::fread(csvname, sep = sep, header = header)

  csv[, (names(csv)[7]) := as.character(get(names(csv)[7]))]
  if (lang == 'en') setcolorder(csv, stColNames)
  if (lang == 'sp') setcolorder(csv, stColNames_sp)

  output <- new(Class = 'StfwfSchema', df = csv)
  return(output)

}
