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
           function(filename, StfwfSchema, encoding = 'unknown', check = FALSE, perl = FALSE) {

             standardGeneric("fread_fwf")

           })

#' @rdname fread_fwf
#'
#' @include StfwfSchema-class.R getdf.R
#'
#' @export
setMethod(f = "fread_fwf",
          signature = c("character", "StfwfSchema"),
          function(filename, StfwfSchema, encoding = 'unknown', check = FALSE, perl = FALSE){

    trim <- function (x) gsub("^\\s+|\\s+$", "", x)

    dt <-  data.table::fread(file = filename, colClasses = "character",
                             sep = "\n", header = FALSE, encoding = encoding)
    schema <- getdf(StfwfSchema)
    posMatrix <- schema[, c('initialPos', 'finalPos')]
    varNames <- schema$variable
    dt[ , (varNames) := lapply(1:(dim(posMatrix)[1]),
                                    function(i) {
                                      stringi::stri_sub(V1,
                                                        posMatrix[i,1],
                                                        posMatrix[i, 2])})][, V1 := NULL]
    dt[, (varNames) := lapply(.SD, trim), .SDcols = varNames]
    numVarNames <- schema$variable[schema$type == 'num']
    dt[, (numVarNames) := lapply(.SD, as.numeric), .SDcols = numVarNames]

    if (check){

      cat('[fastReadfwf:: fread_fwf] Value patterns will be checked for each variable.\n\n')
      varNames <- schema$variable
      for (i in seq(along = varNames)){

        cat(paste0('Checking variable ', varNames[i], '... '))
        pattern <- schema$valueRegEx[i]
        values <- dt[[varNames[i]]]
        wrongValuesindex <- which(regexpr(pattern, values, perl = perl) == -1)
        if (length(wrongValuesindex) == 0) {

          cat('ok.\n')

        } else {

          cat()

          stop(paste0('\nPlease revise either the data set or the regex for this variable.\n\n The following values do not follow the pattern:\n',
                      paste0(dt[[varNames[i]]][wrongValuesindex], collapse = ', ')))
        }
      }
    }


    return(dt[])
})

