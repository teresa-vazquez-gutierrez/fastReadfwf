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
#' @param validate Logical vector of length 1 with default value \code{FALSE} to indicate whether to
#' validate the content of \code{data} before writing.
#'
#' @param justify Character vector of length 1 with default value \code{left} to indicate whether to
#' justify strings to the left or to the right.
#' 
#' @param encoding Character vector of length 1 with default value \code{utf8}, native is also supported.
#'
#' @param ... Other parameters from \code{\link[data.table]{fwrite}}.
#'
#' @return Returns an invisible \code{NULL}. The dataset is written in file \code{filename}.
#'
#' @examples
#' \dontrun{
#' # file will be written to working directory
#' path <- system.file('extdata', package = 'fastReadfwf')
#' stSchema <- fastReadfwf::xlsxToSchema(file.path(path, 'SchemaSNHS.xlsx'), 'stSchema')
#' data(MicroDataSNHS)
#' fwrite_fwf(MicroDataSNHS, file.path(getwd(), 'MicroDataSNHS'), stSchema, justify = 'right')
#'}
#'
#' @seealso \code{\link[data.table]{fwrite}} \code{\link{fread_fwf}]}
#'
#' @import data.table
#'
#' @importFrom stringi stri_join
#'
#' @export
setGeneric("fwrite_fwf",
           function(data, filename, StfwfSchema, validate = FALSE, justify = 'left', ...) {
             standardGeneric("fwrite_fwf")})

#' @rdname fwrite_fwf
#'
#' @include StfwfSchema-class.R getdf.R getVariables.R getTypes.R getWidths.R
#'
#' @export
setMethod(f = "fwrite_fwf",
          signature = c("data.frame", "character", "StfwfSchema"),
          function(data, filename, StfwfSchema, validate = FALSE, justify = 'right', ...){

            value <- auxID <- '.' <- NULL

            if (!justify %in% c('left', 'right')) {

              stop('[fastReadfwf::fwrite_fwf] justify must be either right or left.\n')

            }

            if(validate){

              data <- setChar(data)
              validateValues(data, StfwfSchema)

            }

            data.DT <- as.data.table(data)
            widths <- getWidths(StfwfSchema)
            ColNames <- names(data.DT)
            varNotPresentInSch <- ColNames[which(!ColNames %in% names(widths))]
            if (length(varNotPresentInSch) > 0) {

              stop(paste0('[fastReadfwf::fwrite_fwf] The following variables in data are not present in the schema: ',
                          paste0(varNotPresentInSch, collapse = ', '), '.\n'))

            }
            for (i in seq(along = ColNames)){

              width <- widths[ColNames[i]]
              variable <- ColNames[i]
              data.DT[, (variable) := format(as.character(get(variable)),
                                             width = width,
                                             justify = justify,
                                             na.encode = FALSE)][
                                               is.na(get(variable)), (variable) := paste0(rep(' ', width), collapse = '')]
            }

            varNotPresentInDT <- names(widths)[which(!names(widths) %in% ColNames)]

            if(length(varNotPresentInDT) > 0){

              widths.NotPresentInDT <- widths[varNotPresentInDT]
              auxDT <- data.table(width = unique(widths.NotPresentInDT))
              fWhite <- function(i){sapply(i, function(i) paste0(rep.int(' ', i), collapse = ''))}
              auxDT[, value := fWhite(width)]
              data.DT_NotPresent <- data.table(variable = names(widths.NotPresentInDT), width = widths.NotPresentInDT)
              data.DT_NotPresent <- merge(data.DT_NotPresent, auxDT, by = 'width')[
                , width := NULL][
                , auxID := 'auxID']
              data.DT_NotPresent <- dcast(data.DT_NotPresent, formula = auxID ~ variable, value.var = 'value')[
                , auxID := NULL]
              data.DT[, names(data.DT_NotPresent) := data.DT_NotPresent]

            }

            setcolorder(data.DT, getVariables(StfwfSchema))


            data.DT[, row := Reduce(function(...) stri_join(...), .SD), .SDcols = ColNames][
              , .(row)]

            fwrite(data.DT[, .(row)], filename, sep = '\n', row.names = FALSE, col.names = FALSE, ...)
            cat(paste0('\n data written in ', filename), '\n')


            return(invisible(NULL))

          })

