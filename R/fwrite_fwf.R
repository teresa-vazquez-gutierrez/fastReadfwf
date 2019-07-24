#' @title Fast write a fixed-width file.
#'
#' @description \code{fwrite_fwf} takes as basic input a \linkS4class{data.table} and the schema for
#' the fwf file to write, concatenates columns accordingly and uses \code{\link[data.table]{fwrite}}
#' to write the file on disk.
#'
#' @param data \linkS4class{data.table} with the data to write.
#'
#' @param filename Character vector of length 1 with the name of the file to write.
#'
#' @param StfwfSchema Object of class \linkS4class{StfwfSchema} with the schema of the file.
#'
#' @param justify Character vector of length 1 with default value \code{left} to indicate whether to
#' justify strings to the left or to the right.
#'
#' @param ... Other parameters from \code{\link[data.table]{fwrite}}.
#'
#' @return Returns an invisible \code{NULL}. The dataset is written in file \code{filename}.
#'
#' @examples
#' # file will be written to working directory
#' path <- system.file('extdata', package = 'fastReadfwf')
#' stSchema <- fastReadfwf::xlsxToSchema(file.path(path, 'SchemaSNHS.xlsx'), 'stSchema')
#' data(MicroDataSNHS)
#' fwrite_fwf(MicroDataSNHS, file.path(getwd(),'MicroDataSNHS'), stSchema, justify = 'right')
#'
#' @seealso \code{\link[data.table]{fwrite}} \code{\link{fread_fwf}]}
#'
#' @import data.table
#'
#' @importFrom stringi stri_join
#'
#' @export
setGeneric("fwrite_fwf",
           function(data, filename, StfwfSchema, justify = 'right', ...) {
             standardGeneric("fwrite_fwf")})

#' @rdname fwrite_fwf
#'
#' @include StfwfSchema-class.R getdf.R getVariables.R getTypes.R getWidths.R
#'
#' @export
setMethod(f = "fwrite_fwf",
          signature = c("data.frame", "character", "StfwfSchema"),
          function(data, filename, StfwfSchema, justify = 'right', ...){

          if (!justify %in% c('left', 'right')) {

            stop('[fastReadfwf::fwrite_fwf] justify must be either right or left.')
          }
            data.DT <- as.data.table(data)
            widths <- getWidths(StfwfSchema)
            widths.DT <- data.table(variable = names(widths), width = widths)
            widths.DT <- widths.DT[variable %chin% names(data.DT)]
            ColNames <- names(data.DT)
            for (i in seq(along = ColNames)){

              variable <- ColNames[i]
              data.DT[, (variable) := format(get(variable),
                                        width = widths.DT[variable == variable][['width']],
                                        justify = 'right')]

            }
            data.DT[, row := Reduce(function(...) stri_join(...), .SD), .SDcols = ColNames][
            #data.DT[, row := Reduce(function(...) paste0(...), .SD), .SDcols = ColNames][
              , .(row)]

            fwrite(data.DT[, .(row)], filename, sep = '\n', row.names = FALSE, col.names = FALSE, ...)
            cat(paste0('\ndata written in ', filename), '\n')


            return(invisible(NULL))

})

