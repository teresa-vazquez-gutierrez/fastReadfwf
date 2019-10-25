#' @title Set the class of each variable in data to character.
#'
#' @description \code{setChar} Set the class of each variable in \code{data}
#' to character and the \code{NA} values to empty string.
#'
#' @param data It can be a \linkS4class{data.table} or a \link[tibble]{tibble}.
#'
#' @return Returns a \linkS4class{data.table} or a \link[tibble]{tibble}
#' with the content of \code{data} but with all the columns set to character.
#'
#' @examples
#' path <- system.file('extdata', package = 'fastReadfwf')
#' stSchema <- fastReadfwf::xlsxToSchema(file.path(path, 'SchemaSNHS.xlsx'), 'stSchema')
#'
#' # For data.tables
#' data.DT.char <- fread_fwf(
#'      file.path(path, 'MicroDataSNHS.txt'), stSchema, validate = FALSE, convert = FALSE,
#'        outFormat = 'data.table', perl = TRUE)
#' data.DT.types <- setTypes(data.DT.char, stSchema)
#' data.DT.back <- setChar(data.DT.types)
#' head(data.DT.types)
#'
#' # For tibbles
#'  data.tibble.char <- fread_fwf(
#'       file.path(path, 'MicroDataSNHS.txt'), stSchema, validate = FALSE, convert = FALSE,
#'        outFormat = 'tibble')
#'  data.tibble.types <- setTypes(data.tibble.char, stSchema)
#'  data.tibble.back <- setChar(data.tibble.types)
#'  head(data.tibble.types)
#'
#' @seealso \code{\link[fastReadfwf]{fwrite_fwf}} \code{\link[fastReadfwf]{setTypes}}
#'
#' @import data.table
#'
#' @importFrom stringi stri_sub
#'
#' @importFrom tibble as_tibble
#'
#' @export
setGeneric("setChar",
           function(data) {
             standardGeneric("setChar")})

#' @rdname setTypes
#'
#' @export
setMethod(f = "setChar",
          signature = c("data.frame"),
          function(data){

            if (!any(c('data.table', 'tbl') %in% class(data))){

              stop('[fastReadfwf:: setChar] Data format not supported.\n')

            }

            if ('data.table' %in% class(data)) {

              allVars <- names(data)
              data[, (allVars) := lapply(.SD, as.character), .SDcols = allVars]

              data[is.na(data)] <- ""

              return(data[])

            }

            if ('tbl' %in% class(data)) {

              data <- as.data.table(data)

              allVars <- names(data)
              data[, (allVars) := lapply(.SD, as.character), .SDcols = allVars]

              data[is.na(data)] <- ""

              tibble <- as_tibble(data)

              return(tibble)

            }



          })

