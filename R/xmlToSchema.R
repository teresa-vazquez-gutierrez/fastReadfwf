#' @title Build an object of class \linkS4class{StfwfSchema}.
#' 
#' @description \code{xmlToSchema} is a constructor of the class \linkS4class{StfwfSchema}.
#' 
#' This constructor reads an xml file containing totally the schema of the
#' fixed-width file to read. This file contain a root node named \code{Schema},
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
#' @param inputPath Path of the xml file containing the schema.
#' 
#' @return Return an object of class \linkS4class{StfwfSchema}.
#' 
#' @examples 
#' 
#' inputPath <- file.path(system.file('data', package = 'fastReadfwf'), 'disreg_enceursalud20_a.xml')
#' xmlToSchema(inputPath)
#' 
#' inputPath <- file.path(system.file('data', package = 'fastReadfwf'), 'dr_EPA_2021.xml')
#' xmlToSchema(inputPath)
#' 
#' @importFrom xml2 as_list read_xml
#' 
#' @export

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