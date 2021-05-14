#' @title Create a .xml file 
#' 
#' 
#' @description \code{xlsxToXML} create a .xml file following a certain structure.
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
#' @include formatoR2regex
#' 
#' @export
xlsxToXML <- function(xlsxName, sheetToRead = 1, xmlName, regionName = "METADATOS"){
  #Lectura del xlsx y construccion del xml#
  cat("Leyendo hoja", sheetToRead, "del xlsx:", xlsxName, "...")
  regions_info <- getNamedRegions(xlsxName)
  region_idx  <- which(regions_info == regionName)
  region_cells <- attr(regions_info, "position")[region_idx]
  region_rows <- as.integer(gsub("[A-Z]*", "", strsplit(region_cells, ":")[[1]]))
  
  xlsx <- read.xlsx(xlsxName, sheet=sheetToRead, rows = c(region_rows[1]:region_rows[2]))
  names(xlsx) <- chartr("áéíóú…", "aeiou.", names(xlsx))
  cat(" ok.\n")
  
  cat("Construyendo estructura del Schema...")
  setnames(xlsx, c("Variable", "Longitud", "Posicion", "Descripcion"), 
           c("variable", "width", "initialPos", "description"))
  
  xlsx$finalPos <- xlsx$initialPos + xlsx$width - 1
  xlsx$type <- lapply(xlsx$Tipo, function(xlsxTipo){
    switch(xlsxTipo,
           "A" = "char",
           "N" = "num")
  })
  xlsx$valueRegEx <- formatoR2regex(xlsx$FormatoR)
  
  stColNames <- c("variable","width","initialPos","finalPos","type","valueRegEx",
                  "description")
  xlsx <- xlsx[, stColNames]
  
  cat(" ok.\n")
  
  cat("Construyendo XML...")
  new_xml <- xml2::xml_new_root(
    .value = "Schema",
    .version = "1.0",
    .encoding = "UTF-8")
  
  xml2::xml_add_child(new_xml, .value = "vars")
  
  vars_xml <- lapply(purrr::transpose(xlsx),
                     function(x) {
                       xml2::as_xml_document(list(var = lapply(x, as.list)))
                     })
  
  for(var in vars_xml) xml2::xml_add_child(xml_child(new_xml), var)
  
  cat(" ok.\n")
  
  cat("Guardando XML en ", xmlName, "...")
  xml2::write_xml(new_xml, file = xmlName)
  cat(" ok.\n")

}
