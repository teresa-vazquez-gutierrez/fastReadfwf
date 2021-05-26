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
#' @param xlsxName Complete name of the xlsx file containing the schema.
#' 
#' @param sheetToRead Name or index of the sheet of the xlsx file.
#' 
#' @param xmlName Complete name where the xml file is going to be written.
#' 
#' @param regionName Region from the xlsx which contains the info about the number of rows.
#' 
#' @return Return an object of class \linkS4class{StfwfSchema}.
#' 
#' @examples 
#' path <- 'inst/extdata'
#' xlsxName    <- file.path(system.file('extdata', package = 'fastReadfwf'), 'dr_EESEadulto_2020.xlsx')
#' xmlName     <- file.path(system.file('extdata', package = 'fastReadfwf'), 'dr_EESEadulto_2020.xml')
#' output <- INExlsxToSchema(xlsxName = xlsxName, xmlName = xmlName)
#' 
#' @import data.table
#' 
#' @importFrom openxlsx read.xlsx
#' 
#' @include INExlsxToXML.R INExmlToSchema.R
#' 
#' @export
INExlsxToSchema <- function(xlsxName, sheetToRead = 1, xmlName = NULL, regionName = "METADATOS"){
  
  xmlSchema <- INExlsxToXML(xlsxName = xlsxName, 
                            sheetToRead = sheetToRead, 
                            xmlName = xmlName, regionName = regionName)
  
  output <- INExmlToSchema(xmlSchema = xmlSchema)
  
  return(output)
}