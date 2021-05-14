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
#' @param inputPath Path of the xlsx file containing the schema.
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
#' inputPath    <- file.path(system.file('data', package = 'fastReadfwf'), 'disreg_enceursalud20_a.xlsx')
#' outputPath   <- file.path(system.file('data', package = 'fastReadfwf'), 'disreg_enceursalud20_a.xml')
#' sheetToRead  <- 'Diseño'
#' rowsToDelete <- 2
#' xlsxToXML(inputPath = inputPath, sheetToRead = sheetToRead, outputPath = outputPath, rowsToDelete = rowsToDelete)
#' 
#' inputPath    <- file.path(system.file('data', package = 'fastReadfwf'), 'dr_EPA_2021.xlsx')
#' outputPath   <- file.path(system.file('data', package = 'fastReadfwf'), 'dr_EPA_2021.xml')
#' sheetToRead  <- 'Diseño'
#' rowsToDelete <- 7
#' xlsxToXML(inputPath = inputPath, sheetToRead = sheetToRead, outputPath = outputPath, rowsToDelete = rowsToDelete)
#' 
#' @import data.table
#' 
#' @importFrom openxlsx read.xlsx
#' 
#' @importFrom XML xmlTree addTag closeTag saveXML
#' 
#' @export
xlsxToXML <- function(inputPath, sheetToRead, outputPath, rowsToDelete){
  #Lectura del xlsx y construccion del xml#
  cat("Leyendo hoja", sheetToRead, "del xlsx:", inputPath, "\n")
  xlsx <- read.xlsx(inputPath, sheet=sheetToRead, startRow = 2)
  names(xlsx) <- chartr("ó…", "o.", names(xlsx))
  xlsx <- xlsx[1:(nrow(xlsx)-rowsToDelete),]
  
  cat("Construyendo estructura del Schema... \n")
  setnames(xlsx, c("Variable", "Longitud", "Posicion", "Descripcion"), 
           c("variable", "width", "initialPos", "description"))
  xlsx$finalPos <- xlsx$initialPos + xlsx$width - 1
  xlsx$type <- lapply(xlsx$Tipo, function(xlsxTipo){
    switch(xlsxTipo,
           "A" = "char",
           "N" = "num")
  })
  xlsx$valueRegEx <- xlsx$FormatoR
  stColNames <- c("variable","width","initialPos","finalPos","type","valueRegEx",
                  "description")
  xlsx <- xlsx[, stColNames]
  
  cat("Construyendo XML... \n")
  newXML <- xmlTree()
  newXML$addTag("Schema", close=FALSE)
  newXML$addTag("Variables", close=FALSE)
  lapply(1:nrow(xlsx), function(nrxlsx){
    newXML$addTag("var", close=FALSE)
    lapply(names(xlsx), function(nxlsx){
      newXML$addTag(nxlsx, xlsx[nrxlsx, nxlsx])
    })
    newXML$closeTag()
  })
  newXML$closeTag()
  newXML$closeTag()
  cat("Creando XML en", outputPath, "\n")
  saveXML(newXML, outputPath)
}
