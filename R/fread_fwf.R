#' @title Fast read a fixed-width file
#'
#' @description \code{fread_fwf} takes as basic input a fixed-width filename and its schema and
#' wraps around \code{\link[data.table]{fread}} from package \linkS4class{data.table} to provide the
#' contents of the file.
#'
#' This method converts the slot \code{Data} from the input \code{StQ} object into a
#' \linkS4class{data.table} with statistical units by row and variables specified in the input
#' parameter \code{VarNames} by columns.
#'
#' To distinguish between variables and qualifiers this function makes use of the slot \code{DD} of
#' input \linkS4class{StQ} variable.
#'
#' This method is indeed a wrapper for the function \code{\link[data.table]{dcast.data.table}} of
#' the package \linkS4class{data.table}, adapted to the structure of object \linkS4class{StQ}.
#'
#' @param filename Object of class \linkS4class{StQ} whose slot \code{Data} will be converted.
#'
#' @param schema \code{Character} vector with names of the output variables.
#'
#' @param perl Logical vector of length 1 indicating whether Perl is installed in the system or not.
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
           function(filename, StfwfSchema, perl = FALSE, encoding = 'unknown') {
             standardGeneric("fread_fwf")})

#' @rdname fread_fwf
#'
#' @include StfwfSchema-class.R
#'
#' @export
setMethod(f = "fread_fwf",
          signature = c("character", "StfwfSchema"),
          function(filename, StfwfSchema, perl = FALSE, encoding = 'unknown'){

    dt <-  fread(file = filename, colClasses = "character",
                 sep = "\n", header = FALSE, encoding = encoding)
    schema <- getdf(StfwfSchema)
    posMatrix <- schema[, c('initialPos', 'finalPos')]
    dt[ , schema$variable := lapply(1:(dim(mat)[1]), function(i) {stringi::stri_sub(V1, mat[i,1], mat[i, 2])})][, V1 := NULL]
    return(dt[])
})

