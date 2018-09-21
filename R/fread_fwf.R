#' @title Fast read a fixed-width file
#'
#' @description \code{fread_fwf} takes as basic input a fixed-width filename and its schema and
#' wraps around \code{\link[data.table]{fread}} from package \linkS4class{data.table} to provide the
#' contents of the file.
#'
#' This method is indeed a wrapper for the function \code{\link[data.table]{fread}} followed by the
#' application of \code{\link[stringi]{stri_sub}}.
#'
#' @param filename Character vector of length 1 with the name of the file to read.
#'
#' @param StfwfSchema Object of class \linkS4class{StfwfSchema} with the schema of the file to read.
#'
#' @param encoding Character vector of length 1 with default value is "unknown". Other possible
#' options are "UTF-8" and "Latin-1".
#' NB: it is not used to re-encode the input but to enable handling of encoded strings in their
#' native encoding.
#'
#' @return Returns a \linkS4class{data.table} with the contents of the file.
#'
#' @examples
#' \dontrun{
#' filename <- 'file1.txt'
#' fileschema <- XLSToSchema('file1schema.xlsx')
#' fread_fwf(filename, fileschema)
#'
#' }
#' @seealso \code{\link[data.table]{fread}}
#'
#' @import data.table
#'
#' @importFrom stringi stri_sub
#'
#' @export
setGeneric("fread_fwf",
           function(filename, StfwfSchema, encoding = 'unknown') {standardGeneric("fread_fwf")})

#' @rdname fread_fwf
#'
#' @include StfwfSchema-class.R
#'
#' @export
setMethod(f = "fread_fwf",
          signature = c("character", "StfwfSchema"),
          function(filename, StfwfSchema, encoding = 'unknown'){

    dt <-  fread(file = filename, colClasses = "character",
                 sep = "\n", header = FALSE, encoding = encoding)
    schema <- getdf(StfwfSchema)
    posMatrix <- schema[, c('initialPos', 'finalPos')]
    dt[ , schema$variable := lapply(1:(dim(posMatrix)[1]), function(i) {stringi::stri_sub(V1, posMatrix[i,1], posMatrix[i, 2])})][, V1 := NULL]
    return(dt[])
})

