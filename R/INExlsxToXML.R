#' @title Create an xml file with the contents of an INE xlsx schema file.
#' 
#' 
#' @description \code{xlsxToXML} creates an xml file with the contents of the
#' xlsx file by the Spanish NSI standard for the fwf schema.
#' 
#' The Excel file must have a header in the second row. 
#' 
#' @param xlsxName Name of the xlsx file containing the schema.
#' 
#' @param sheetToRead Name or index of the sheet of the xlsx file.
#' 
#' @param xmlName Name of the xml file is going to be written.
#' 
#' @param regionName Name of the region in the xlsx file.
#'
#' @return Write the generated xml file.
#' 
#' @examples 
#' xlsxName    <- file.path(system.file('inst/extdata', package = 'fastReadfwf'), 'dr_EESEadulto_2020.xlsx')
#' xmlName     <- file.path(system.file('inst/extdata', package = 'fastReadfwf'), 'dr_EESEadulto_2020.xml')
#' sheetToRead <- 'Diseño'
#' INExlsxToXML(xlsxName = xlsxName, sheetToRead = sheetToRead, xmlName = xmlName)
#' 
#' xlsxName    <- file.path(system.file('data', package = 'fastReadfwf'), 'dr_EPA_2021.xlsx')
#' outputPath   <- file.path(system.file('data', package = 'fastReadfwf'), 'dr_EPA_2021.xml')
#' sheetToRead  <- 'Diseño'
#' INExlsxToXML(xlsxName = xlsxName, sheetToRead = sheetToRead, outputPath = outputPath, rowsToDelete = rowsToDelete)
#' 
#' @import data.table
#' 
#' @include formatoR2regex.R
#' 
#' @export
INExlsxToXML <- function(xlsxName, sheetToRead = 1, xmlName, regionName = "METADATOS"){
  #Lectura del xlsx y construccion del xml#
  cat("Leyendo hoja", sheetToRead, "del xlsx:", xlsxName, "...")
  regions_info <- getNamedRegions(xlsxName)
  region_idx  <- which(regions_info == regionName)
  region_cells <- attr(regions_info, "position")[region_idx]
  region_rows <- as.integer(gsub("[A-Z]*", "", strsplit(region_cells, ":")[[1]]))
  
  xlsx <- openxlsx::read.xlsx(
    xlsxName, sheet=sheetToRead, rows = c(region_rows[1]:region_rows[2]))
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
  
  return(new_xml)

}
