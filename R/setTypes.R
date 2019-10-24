#' @title Set the types in the schema as class of each variable in data.
#'
#' @description \code{setTypes} Set the types in the \code{schema} as class of
#' each variable in \code{data}.
#'
#' @param data It can be a \linkS4class{data.table} or a \link[tibble]{tibble}
#' with all its columns of class character.
#'
#' @param StfwfSchema Object of class \linkS4class{StfwfSchema} with the schema
#' used to read the object \code{data}.
#'
#' @return Returns a \linkS4class{data.table} or a \link[tibble]{tibble}
#' with the content of \code{data} but with the columns set to the class corresponding
#' to the types in \linkS4class{StfwfSchema}.
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
#' head(data.DT.types)
#'
#' # For tibbles
#'  data.tibble.char <- fread_fwf(
#'       file.path(path, 'MicroDataSNHS.txt'), stSchema, validate = FALSE, convert = FALSE,
#'        outFormat = 'tibble')
#'  data.tibble.types <- setTypes(data.tibble.char, stSchema)
#'  head(data.tibble.types)
#'
#' @seealso \code{\link[fastReadfwf]{fread_fwf}}
#'
#' @include StfwfSchema-class.R getVariables.R getTypes.R
#'
#' @import data.table
#'
#' @importFrom stringi stri_sub
#'
#' @importFrom tibble as_tibble
#'
#'
#' @export
setGeneric("setTypes",
           function(data, StfwfSchema) {
             standardGeneric("setTypes")})

#' @rdname setTypes
#'
#' @export
setMethod(f = "setTypes",
          signature = c("data.frame", "StfwfSchema"),
          function(data, StfwfSchema){

            if (!any(c('data.table', 'tbl') %in% class(data))){

              stop('[fastReadfwf:: setTypes] Data format not supported.\n')

            }

            types <- fastReadfwf::getTypes(StfwfSchema)
            varNames <- fastReadfwf::getVariables(StfwfSchema)

            numVarNames <- varNames[types == 'num']

            if ('data.table' %in% class(data)) {

              indx2 <- which(names(data) %in% numVarNames)
              for (j in indx2) set(data, i = grep("^$|^ $", data[[j]]), j = j, value = NA_character_)

              if (length(numVarNames) > 0) {

                data[, (numVarNames) := lapply(.SD, as.numeric), .SDcols = numVarNames]

              }
              return(data[])

            }

            if ('tbl' %in% class(data)) {

              data <- as.data.table(data)

              indx2 <- which(names(data) %in% numVarNames)
              for (j in indx2) set(data, i = grep("^$|^ $", data[[j]]), j = j, value = NA_character_)

              if (length(numVarNames) > 0) {

                data[, (numVarNames) := lapply(.SD, as.numeric), .SDcols = numVarNames]

              }
              tibble <- as_tibble(data)
              return(tibble)

            }

          })

