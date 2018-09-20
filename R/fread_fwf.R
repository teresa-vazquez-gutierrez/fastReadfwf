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
#'
#' @seealso \code{\link[data.table]{fread}}
#'
#' @import data.table
#'
#' @importFrom stringi stri_sub
#'
#' @export
setGeneric("fread_fwf", function(filename, schema) {standardGeneric("fread_fwf")})

#' @rdname fread_fwf
#'
#' @include StfwfSchema-class.R
#'
#' @export
setMethod(f = "fread_fwf",
          signature = c("character", "StfwfSchema"),
          function(filename, schema){

    sc <- read.table(file= schema, header= FALSE, sep= ";") #schema fwf
    variables <- as.character(sc[, 1])  ##Variables
    start_col <- sc[,2] ##Posicion inicial
    end_col <- sc[,3] ##Posicion inicial
    start_end <- cbind(start_col, end_col)

    dt <-  fread(file = file,
                 colClasses = "character",
                 sep = "\n",
                 header = FALSE)

    extrae <- function(x) {
    apply(start_end, 1, function(y) stringi::stri_sub(x, y[1], y[2]))}

    dt[, (variables) := data.table((lapply(dt, extrae))$V1)] [, V1 := NULL]

    rm(extrae)

    return(dt)
})

