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
#' @param xmlName Path of the xml file containing the schema.
#' 
#' @param xmlSchema Object of class \code{\link[xml2]{xml_document-class}} containing the schema.
#' 
#' @return Return an object of class \linkS4class{StfwfSchema}.
#' 
#' @examples 
#' xmlName <- file.path(system.file('extdata', package = 'fastReadfwf'), 'dr_EESEadulto_2020.xml')
#' output <- INExmlToSchema(xmlName)
#' 
#' @import data.table
#' 
#' @importFrom purrr map_df
#' 
#' @export
INExmlToSchema <- function(xmlName = NULL, xmlSchema = NULL){
  
  if(is.null(xmlName) & is.null(xmlSchema)){
    
    stop(paste0('[fastReadfwf::INExmlToSchema] No xml specified. Both xmlName and xmlSchema are NULL\n.'))
    
  }
  
  width <- finalPos <- initialPos <- NULL
  
  if(!is.null(xmlName)){
    
    #Lectura del XML 
    doc <- xml2::read_xml(xmlName)
    
  }
  if(is.null(xmlName)){ doc <- xmlSchema}
  
  # construcción de la tabla
  nodes <- xml2::xml_find_all(doc, ".//vars/var")
  
  vars.dt <- as.data.table(
    purrr::map_df(nodes, function(x) {
    kids <- xml2::xml_children(x)
    stats::setNames(as.list(xml2::xml_text(kids)), xml2::xml_name(kids))
  })
  )
  vars.dt <- utils::type.convert(vars.dt, as.is = TRUE)
  
  # Comprobación de coherencia de posiciones y width
  check.idx <- which(vars.dt[, width] != (vars.dt[, finalPos] - vars.dt[, initialPos] + 1))
  if(length(check.idx) > 0){
    
    stop(paste0("Las siguientes variables tienen incoherencia entre las posiciones y las anchuras: ",
                paste(vars.dt[check.idx]$variable, collapse = ", "), ".\n"))
    
  }
  
  output <- new(Class = 'StfwfSchema', df = vars.dt)
  return(output)
}