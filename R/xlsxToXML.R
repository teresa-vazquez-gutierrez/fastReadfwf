#====================================#
##########Carga de librerias##########
#====================================#
library(openxlsx)
library(XML)
library(xml2)
library(data.table)

#====================================#
##########xlsxToXML##########
#====================================#
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

#====================================#
##########xmlToSchema##########
#====================================#

xmlToSchema <- function(inputPath){
  #Lectura del XML y construccion del Schema#
  xmlFile <- as_list(read_xml(inputPath))
  nVar <- length(xmlFile$Schema$Variables)
  
  xml.list <- data.frame(matrix(ncol=7, nrow=nVar))
  colnames(xml.list) <- stColNames
  
  for (i in 1:nVar){
    xml.list$variable[i]    <- unlist(xmlFile$Schema$Variables[i]$var$variable)
    xml.list$width[i]       <- unlist(xmlFile$Schema$Variables[i]$var$width)
    xml.list$initialPos[i]  <- unlist(xmlFile$Schema$Variables[i]$var$initialPos)
    xml.list$finalPos[i]    <- unlist(xmlFile$Schema$Variables[i]$var$finalPos)
    xml.list$type[i]        <- unlist(xmlFile$Schema$Variables[i]$var$type)
    xml.list$valueRegEx[i]  <- unlist(xmlFile$Schema$Variables[i]$var$valueRegEx)
    xml.list$description[i] <- unlist(xmlFile$Schema$Variables[i]$var$description)
  }
  
  n <- dim(xml.list)[1]
  
  # Classes of columns width, initialPos, finalPos must be integer
  xml.list$width <- as.integer(xml.list$width)
  widthNAs <- is.na(xml.list$width)
  invalidWidths <- xml.list[widthNAs, 'variable']
  if (sum(widthNAs) != 0 & sum(widthNAs) != n) {
    
    stop(
      paste0(
        '[fastReadfwf::xlsxToSchema] The following variables have wrong width: ',
        paste0(invalidWidths, collapse = ', '), '.\n'))
  }
  
  xml.list$initialPos <- as.integer(xml.list$initialPos)
  initialPosNAs <- is.na(xml.list$initialPos)
  invalidinitialPos <- xml.list[initialPosNAs, 'variable']
  if (sum(initialPosNAs) != 0 & sum(initialPosNAs) != n) {
    
    stop(paste0(
      '[fastReadfwf::xlsxToSchema] The following variables have wrong initial positions: ',
      paste0(invalidinitialPos, collapse = ', '), '.\n'))
  }
  
  xml.list$finalPos <- as.integer(xml.list$finalPos)
  finalPosNAs <- is.na(xml.list$finalPos)
  invalidfinalPos <- xml.list[finalPosNAs, 'variable']
  if (sum(finalPosNAs) != 0 & sum(finalPosNAs) != n) {
    
    stop(paste0(
      '[fastReadfwf::xlsxToSchema] The following variables have wrong final positions: ',
      paste0(invalidfinalPos, collapse = ', '), '.\n'))
  }
  
  # No initialPos and no finalPos: only width specified
  if (all(!is.na(xml.list$width)) & all(is.na(xml.list$finalPos)) & all(is.na(xml.list$initialPos))) {
    
    xml.list$initialPos <- 1 + c(0, cumsum(xml.list$width)[-n])
    xml.list$finalPos <- xml.list$initialPos + xml.list$width - 1
  }
  
  # Whitespaces to .*
  xml.list$valueRegEx[is.na(xml.list$valueRegEx) | xml.list$valueRegEx == ''] <- '.*'
  
  output <- new(Class = 'StfwfSchema', df = xml.list)
  return(output)
}

#====================================#
##########xlsxToXMLToSchema##########
#====================================#

xlsxToXMLToSchema <- function(xlsxPath, sheetToRead, xmlPath, rowsToDelete){
  
  xlsxToXML(inputPath = xlsxPath, 
            sheetToRead = sheetToRead, 
            outputPath = xmlPath, rowsToDelete = rowsToDelete)
  
  output <- xmlToSchema(inputPath = xmlPath)
}


#====================================#
##########Prueba de funciones##########
#====================================#
#Parámetros de prueba
inputPath    <- "C:/Users/jorge/Desktop/Beca_INE/data/disreg_enceursalud20_a.xlsx"
outputPath   <- "C:/Users/jorge/Desktop/Beca_INE/data/disreg_enceursalud20_a.xml"
sheetToRead  <- "Diseño"
rowsToDelete <- 2

#inputPath    <- "C:/Users/jorge/Desktop/Beca_INE/data/dr_EPA_2021.xlsx"
#outputPath   <- "C:/Users/jorge/Desktop/Beca_INE/data/dr_EPA_2021.xml"
#sheetToRead  <- "Diseño"
#rowsToDelete <- 7

xlsxToXML(inputPath = inputPath, 
          sheetToRead = sheetToRead, 
          outputPath = outputPath, rowsToDelete = rowsToDelete)

outSchema <- xmlToSchema(inputPath = outputPath)

outSchema2 <- xlsxToXMLToSchema(inputPath, sheetToRead, outputPath, rowsToDelete)