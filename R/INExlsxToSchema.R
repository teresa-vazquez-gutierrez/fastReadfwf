#' @title Transform Spanish NSI fwf schema into an object of class \linkS4class{StfwfSchema}.
#' 
#' @description \code{INExlsxToSchema} transforms an xlsx file in the Spanish NSI internal standard
#' for fwf schemas into an object with class \linkS4class{StfwfSchema}.
#' 
#' This function reads an Excel file containing the schema of the fixed-width file to read according
#' to the Spanish NSI internal standard, creates an intermediate xml file with the information of 
#' the schema, reads this xml file and constructs an object of class \linkS4class{StfwfSchema}. The
#' intermediate xml file contains the following elements:
#' 
#' \itemize{
#'
#'    \item \code{variable}: the name of the variable.
#'    \item \code{width}: the number of positions which the values of this variable occupy in the 
#'    fwf file.
#'    \item \code{initialPos}: initial position of the field which the values of this variable 
#'    occupy in the fwf file.
#'    \item \code{finalPos}: final position of the field which the values of this variable occupy in
#'     the fwf file.
#'    \item \code{type}: type of the variable. It must be either \code{log}, \code{integer}, 
#'    \code{num} or \code{char}.
#'    \item \code{valueRegEx}: regular expression for the values of this variable.
#'    \item \code{description}: textual description of the variable.
#'
#' }
#' 
#' The Excel file must have a header in the second row (according to Spanish NSI internal standard).. 
#' 
#' @param xlsxName Complete name of the xlsx file containing the schema.
#' 
#' @param sheetToRead Name or index of the sheet of the schema in the xlsx file.
#' 
#' @param xmlName Complete name where the xml file is going to be written.
#' 
#' @param regionName Region from the xlsx file containing the info about the number of rows.
#' 
#' @return Return an object of class \linkS4class{StfwfSchema}.
#' 
#' @seealso \code{\link{INExlsxToXML}} \code{\link{INExmlToSchema}} 
#' 
#' @examples 
#' path <- 'inst/extdata'
#' xlsxName <- file.path(system.file('extdata', package = 'fastReadfwf'), 'dr_EESEadulto_2020.xlsx')
#' xmlName  <- file.path(system.file('extdata', package = 'fastReadfwf'), 'dr_EESEadulto_2020.xml')
#' stSchema <- INExlsxToSchema(xlsxName = xlsxName, xmlName = xmlName)
#' 
#' @import data.table
#' 
#' @importFrom openxlsx read.xlsx
#' 
#' @include INExlsxToXML.R INExmlToSchema.R
#' 
#' @export
INExlsxToSchema <- function(xlsxName, sheetToRead = 1, xmlName = NULL, regionName = "METADATOS"){
  
  xmlSchema <- INExlsxToXML(xlsxName = xlsxName, sheetToRead = sheetToRead, 
                            xmlName = xmlName, regionName = regionName)
  
  output <- INExmlToSchema(xmlSchema = xmlSchema)
  
  return(output)
}