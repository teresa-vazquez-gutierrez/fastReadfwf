#' @title Create a xml file and build an object of class \linkS4class{StfwfSchema}.
#' 
#' @description \code{xlsxToXMLToSchema} creates a xml file following a certain structure
#' and constructs the class \linkS4class{StfwfSchema}.
#' 
#' This function reads an Excel file containing partially the schema of the fixed-width
#' file to read and creates the xml, then it reads that xml which contain a root node named \code{Schema},
#' a secondary root node named \code{Variables} and one node \code{var} for each
#' variable of the Schema who has the following child nodes:
#' 
#' \itemize{
#'
#'    \item \code{variable}: the name of the variable.
#'    \item \code{width}: the number of positions which the values of
#'    this variable occupies in the file.
#'    \item \code{initialPos}: initial position of the field which
#'    the values of this variable occupies in the file.
#'    \item \code{finalPos}: final position of the field which the
#'    values of this variable occupies in the file.
#'    \item \code{type}: type of the variable. It must be either \code{log},
#'    \code{integer}, \code{num} or \code{char}.
#'    \item \code{valueRegEx}: regular expression for the values of
#'    this variable.
#'    \item \code{description}: textual description of the variable.
#'
#' }
#' 
#' The Excel file must have a header in the second row. 
#' 
#' @param xlsxPath Path of the xlsx file containing the schema.
#' 
#' @param sheetToRead Name or index of the sheet of the xlsx file.
#' 
#' @param xmlPath Path where the xml file is going to be written.
#' 
#' @param rowsToDelete Last rows on the sheet that don't belong to the schema data table.
#' 
#' @return Return an object of class \linkS4class{StfwfSchema}.
#' 
#' @examples 
#' inputPath    <- file.path(system.file('data', package = 'fastReadfwf'), 'disreg_enceursalud20_a.xlsx')
#' outputPath   <- file.path(system.file('data', package = 'fastReadfwf'), 'disreg_enceursalud20_a.xml')
#' sheetToRead  <- 'Dise?o'
#' rowsToDelete <- 2
#' xlsxToXMLToSchema(xlsxPath = inputPath, sheetToRead = sheetToRead, xmlPath = outputPath, rowsToDelete = rowsToDelete)
#' 
#' inputPath    <- file.path(system.file('data', package = 'fastReadfwf'), 'dr_EPA_2021.xlsx')
#' outputPath   <- file.path(system.file('data', package = 'fastReadfwf'), 'dr_EPA_2021.xml')
#' sheetToRead  <- 'Dise?o'
#' rowsToDelete <- 7
#' xlsxToXMLToSchema(xlsxPath = inputPath, sheetToRead = sheetToRead, xmlPath = outputPath, rowsToDelete = rowsToDelete)
#' 
#' @import data.table
#' 
#' @importFrom openxlsx read.xlsx
#' 
#' @include INExlsxToXML.R
#' 
#' @export
INExlsxToSchema <- function(xlsxPath, sheetToRead, xmlPath, rowsToDelete){
  
  INExlsxToXML(inputPath = xlsxPath, 
               sheetToRead = sheetToRead, 
               outputPath = xmlPath, rowsToDelete = rowsToDelete)
  
  output <- xmlToSchema(inputPath = xmlPath)
  return(output)
}