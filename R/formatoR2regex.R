#' @title Convert FormatoR to regex
#' 
#' 
#' @description \code{formatoR2regex} Convert FormatoR to regex
#'
#' This function reads an Excel file containing partially the schema of the fixed-width
#' file to read. This file must contain the following named columns: 
#'
#'
#' \itemize{
#'
#'    \item \code{Variable}: the name of the variable.
#'    \item \code{Longitud}: the number of positions which the values of
#'    this variable occupies in the file.
#'    \item \code{Posicion} or \code{Posición}: initial position of the field which
#'    the values of this variable occupies in the file.
#'    \item \code{Tipo}: type of the variable. It must be either \code{A} or \code{N}.
#'    \item \code{FormatoR}: regular expression for the values of
#'    this variable.
#'    \item \code{Descripcion} or \code{Descripción}: textual description of the variable.
#'
#' }
#' 
#' The Excel file must have a header in the second row. 
#' 
#' @param xlsxName Path of the xlsx file containing the schema.
#' 
#' @param sheetToRead Name or index of the sheet of the xlsx file.
#' 
#' @param outputPath Path where the xml file is going to be written.
#' 
#' @param rowsToDelete Last rows on the sheet that don't belong to the schema data table.
#'
#' @return Write the generated xml file in the outputPath.
#' 
#' @examples 
#' xlsxName    <- file.path(system.file('data', package = 'fastReadfwf'), 'disreg_enceursalud20_a.xlsx')
#' outputPath   <- file.path(system.file('data', package = 'fastReadfwf'), 'disreg_enceursalud20_a.xml')
#' sheetToRead  <- 'Diseño'
#' rowsToDelete <- 2
#' xlsxToXML(xlsxName = xlsxName, sheetToRead = sheetToRead, outputPath = outputPath, rowsToDelete = rowsToDelete)
#' 
#' xlsxName    <- file.path(system.file('data', package = 'fastReadfwf'), 'dr_EPA_2021.xlsx')
#' outputPath   <- file.path(system.file('data', package = 'fastReadfwf'), 'dr_EPA_2021.xml')
#' sheetToRead  <- 'Diseño'
#' rowsToDelete <- 7
#' xlsxToXML(xlsxName = xlsxName, sheetToRead = sheetToRead, outputPath = outputPath, rowsToDelete = rowsToDelete)
#' 
#' @import data.table
#' 
#' @importFrom openxlsx read.xlsx
#' 
#' @importFrom XML xmlTree addTag closeTag saveXML
#' 
#' @export
formatoR2regex <- function(formatoR){
  
  types <- gsub("[[:digit:]]", "", formatoR)
  types <- gsub("[[:punct:]]", "", types)
  
  types.regex <- sapply(types, function(x){
    switch(x,
           "A" = "[a-zA-Z]", 
           "I" = "[0-9]",
           "F" = "[0-9]",
           "[.]*")
  })
  
  widths <- gsub("[[:alpha:]]", "", formatoR)
  widths <- strsplit(widths, ".", fixed = TRUE)
  
  widths.regex <- sapply(widths, function(x){
    if(length(x) == 0){output <- ""}
    if(length(x) == 1){
      output <- paste0("{1,", x, "}")
    }
    if(length(x) == 2){
      a <- as.integer(x[1]) - as.integer(x[2])
      output <- paste0("{1,", a, "}", "\\.[0-9]{1,", x[2], "}")
    }
    if(length(x) > 2){stop("Hay un elemento en formatoR no reconocido.")}
    return(output)
  })
  
  output <- paste0(types.regex, widths.regex)
  
  return(output)
  
}