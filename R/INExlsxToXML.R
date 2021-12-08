#' @title Transform Spanish NSI fwf schema into an xml file.
#' 
#' 
#' @description \code{xlsxToXML} transforms an xlsx file in the Spanish NSI internal standard
#' for fwf schemas into an xml file with the following elements:
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
#' @param xlsxName Name of the xlsx file containing the schema according to the Spanish NSI standard
#' 
#' @param sheetToRead Name or index of the sheet in the xlsx file with the schema.
#' 
#' @param xmlName Name of the xml file to be written.
#' 
#' @param regionName Name of the region in the xlsx file.
#'
#' @return Write the generated xml file and return an object of class 
#' \code{\link[xml2]{xml_document-class}}.
#' 
#' @seealso \code{\link{INExlsxToSchema}} \code{\link{INExmlToSchema}}
#' 
#' @examples 
#' xlsxName    <- file.path(system.file('extdata', package = 'fastReadfwf'), 'dr_EESEadulto_2020.xlsx')
#' xmlName     <- file.path(system.file('extdata', package = 'fastReadfwf'), 'dr_EESEadulto_2020.xml')
#' output <- INExlsxToXML(xlsxName = xlsxName, xmlName = xmlName)
#' 
#' @import data.table openxlsx xml2
#' 
#' @include formatoR2regex.R
#' 
#' @export
INExlsxToXML <- function(xlsxName, sheetToRead = 1, xmlName = NULL, regionName = "METADATOS"){
  #Lectura del xlsx y construccion del xml#
  cat("Leyendo hoja", sheetToRead, "del xlsx:", xlsxName, "...")
  regions_info <- openxlsx::getNamedRegions(xlsxName)
  region_idx  <- which(regions_info == regionName)
  region_cells <- attr(regions_info, "position")[region_idx]
  region_rows <- as.integer(gsub("[A-Z]*", "", strsplit(region_cells, ":")[[1]]))
  
  xlsx <- openxlsx::read.xlsx(
    xlsxName, sheet=sheetToRead, rows = c(region_rows[1]:region_rows[2]))
  convertedNames <- iconv(names(xlsx), from = 'UTF-8', to = 'latin1')
  names(xlsx) <- chartr(intToUtf8(c(225, 233, 237, 243, 250)),
                        "aeiou", convertedNames)
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
  
  for(var in vars_xml) xml2::xml_add_child(xml2::xml_child(new_xml), var)
  
  cat(" ok.\n")
  
  if(!is.null(xmlName)){
    
    cat("Guardando XML en ", xmlName, "...")
    xml2::write_xml(new_xml, file = xmlName)
    cat(" ok.\n")
    
  }
  
  
  return(new_xml)

}
