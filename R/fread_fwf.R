#' @title Fast read a fixed-width file.
#'
#' @description \code{fread_fwf} takes as basic input a fixed-width filename and its schema and
#' wraps around \code{\link[data.table]{fread}} from package \linkS4class{data.table} or around
#' \code{\link[readr]{read_fwf}} to provide the contents of the file.
#'
#' This method is indeed either (i) a wrapper for the function \code{\link[data.table]{fread}}
#' followed by the application of \code{\link[stringi]{stri_sub}} or (ii) a direct wrapper for the
#' function \code{\link[readr]{read_fwf}}.
#'
#' @param filename Character vector of length 1 with the name of the file to read.
#'
#' @param StfwfSchema Object of class \linkS4class{StfwfSchema} with the schema of the file to read.
#'
#' @param outFormat Character vector of length 1 whose default value is "data.table". Other
#' option is "tibble".
#'
#' @param perl Logical vector of length 1 with default value \code{FALSE} to indicate whether to use
#' perl or not in the application of the column \code{regexp}.
#'
#' @param ... Other parameters from \code{\link[data.table]{fread}} or \code{\link[readr]{read_fwf}}
#' according to the value of \code{outFormat} above.
#'
#' @return Returns a \linkS4class{data.table} or a \link[tibble]{tibble} with the contents of the
#' file.
#'
#' @examples
#' path <- system.file('extdata', package = 'fastReadfwf')
#' stSchema <- fastReadfwf::xlsxToSchema(file.path(path, 'SchemaSNHS.xlsx'), 'stSchema')
#'
#' # For data.tables
#' data.DT <- fread_fwf(
#' file.path(path, 'MicroDataSNHS.txt'), stSchema, outFormat = 'data.table', perl = TRUE)
#' head(data.DT)
#'
#' # For tibbles
#' data.tibble <- fread_fwf(
#' file.path(path, 'MicroDataSNHS.txt'), stSchema, outFormat = 'tibble')
#' head(data.tibble)
#'
#' @seealso \code{\link[data.table]{fread}}
#'
#' @import data.table
#'
#' @importFrom stringi stri_sub
#'
#' @importFrom readr read_fwf fwf_widths
#'
#' @export
setGeneric("fread_fwf",
           function(filename, StfwfSchema, outFormat, perl = FALSE, ...) {
             standardGeneric("fread_fwf")})

#' @rdname fread_fwf
#'
#' @include StfwfSchema-class.R getdf.R getVariables.R getTypes.R
#'
#' @export
setMethod(f = "fread_fwf",
          signature = c("character", "StfwfSchema"),
          function(filename, StfwfSchema, outFormat = 'data.table', perl = FALSE, ...){

    supportedFormats <- c('data.table', 'tibble')

    if (!outFormat %in% supportedFormats) stop('[fastReadfwf:: fread_fwf] Output format not supported.\n')

    if (outFormat == 'data.table') {

      trim <- function (x) gsub("^\\s+|\\s+$", "", x, perl = perl)
      dt <-  fread(file = filename, colClasses = "character", sep = "\n", header = FALSE, ...)
      schema <- getdf(StfwfSchema)
      posMatrix <- schema[, c('initialPos', 'finalPos')]
      varNames <- getVariables(StfwfSchema)
      dt[ , (varNames) := lapply(1:(dim(posMatrix)[1]),
                                      function(i) {
                                        stringi::stri_sub(V1,
                                                          posMatrix[i,1],
                                                          posMatrix[i, 2])})][, V1 := NULL]
      dt[, (varNames) := lapply(.SD, trim), .SDcols = varNames]

      types <- getTypes(StfwfSchema)
      numVarNames <- varNames[types == 'num']

      indx2 <- which(names(dt) %in% numVarNames)
      for (j in indx2) set(dt, i = grep("^$|^ $", dt[[j]]), j = j, value = NA_character_)

      if (length(numVarNames) > 0) {

        dt[, (numVarNames) := lapply(.SD, as.numeric), .SDcols = numVarNames]

      }
      return(dt[])

    }

    if (outFormat == 'tibble') {

      widths <- getWidths(StfwfSchema)
      varNames <- getVariables(StfwfSchema)
      types <- getTypes(StfwfSchema)
      types <- paste0(substr(types, 1, 1), collapse = '')
      tibble <- readr::read_fwf(
        file = filename,
        col_positions = readr::fwf_widths(widths, varNames),
        col_types = types,
        ...)
      return(tibble)

    }



})

