#' @title Fast read a fixed-width file
#'
#' @description \code{fread_fwf} takes as basic input a fixed-width filename and its schema and
#' wraps around \code{\link[data.table]{fread}} from package \linkS4class{data.table} to provide the
#' contents of the file.
#'
#' This method is indeed a wrapper for the function \code{\link[data.table]{fread}} followed by the
#' application of \code{\link[stringi]{stri_sub}}.
#'
#' @param object Object with rectangular shape whose columns will be validated.
#'
#' @param StfwfSchema Object of class \linkS4class{StfwfSchema} with the schema of object.
#'
#' @param file File to print output to with default value \code{""}.
#'
#' @param id_var Data frame with variables to identify observations with wrong values,
#' with default value \code{NULL}. If not defined, index is used instead.
#'
#' @param analyseAll Logical with default value \code{TRUE} to indicate whether to check
#' all variables with invalid values or not (stopping at the first variable with some invalid value)
#'
#' @param perl Logical vector of length 1 with default value \code{FALSE} to indicate whether to use
#' perl or not in the application of regexp.
#'
#' @return Returns \code{TRUE}.
#'
#' @examples
#' path <- system.file('extdata', package = 'fastReadfwf')
#' stSchema <- fastReadfwf::StxlsxToSchema(
#'    file.path(path, 'SchemaSNHS.xlsx'),
#'    sheetToRead = 'stSchema')
#'
#' # For data.tables
#' data <- fread_fwf(
#' file.path(path, 'MicroDataSNHS.txt'), stSchema, outFormat = 'data.table', perl = TRUE)
#' validateValues(data, stSchema, perl = TRUE)
#'
#' # For tibbles
#' data <- fread_fwf(
#' file.path(path, 'MicroDataSNHS.txt'), stSchema, outFormat = 'tibble')
#' validateValues(data, stSchema, perl = TRUE)
#'
#'
#' @import data.table
#'
#' @include StfwfSchema-class.R getVariables.R getRegEx.R
#'
#' @export
setGeneric("validateValues",
           function(object, StfwfSchema, file = "", id_var = NULL, analyseAll = TRUE, 
                    perl = FALSE) {standardGeneric("validateValues")})

#' @rdname validateValues
#'
#' @import rlang
#'
#' @export
setMethod(f = "validateValues",
          signature = c("data.frame", "StfwfSchema"),
          function(object, StfwfSchema, file = "", id_var = NULL, analyseAll = TRUE, 
                   perl = FALSE){
            
            cat("[fastReadfwf:: validateValues] Value patterns will be checked for each variable.\n\n", file = file, append = FALSE)
            varNames <- getVariables(StfwfSchema)
            valueRegEx <- getRegEx(StfwfSchema)
            valid <- TRUE
            print_idvar <- is.data.frame(id_var) && ncol(id_var) > 0 && nrow(id_var) == nrow(object)
            wrongValuesList <- list()
            
            lapply(seq(along = varNames), function(i){
              cat(paste0('Checking variable ', varNames[i], '... '), file = file, append = TRUE)
              pattern <- valueRegEx[i]
              values <- object[[varNames[i]]]
              wrongValuesindex <- which(regexpr(pattern, values, perl = perl) == -1)
              
              if (length(wrongValuesindex) == 0) {
                cat('OK.\n', file = file, append = TRUE)
              } else {
                cat(paste0('\n  Please revise either the data set or the regex for this variable.\n  The following values do not follow the pattern ', valueRegEx[i], ':\n'), file = file, append = TRUE)
                for (index in wrongValuesindex){
                  if (print_idvar){
                    cat(paste0('    ', paste(colnames(id_var), id_var[index], sep = ": ", collapse = ", "), ', value: ', values[index], '\n'), file = file, append = TRUE)
                  } else{
                    cat(paste0('    Index: ', index, ', value: ', values[index], '\n'), file = file, append = TRUE)
                  }
                }
                if (print_idvar){
                  wvlistItem <- cbind(id_var[wrongValuesindex,], values[wrongValuesindex])
                  colnames(wvlistItem)[length(colnames(wvlistItem))] <- varNames[i]
                  wrongValuesList[[varNames[i]]] <<- wvlistItem
                } else{
                  wvlistItem <- cbind(wrongValuesindex, values[wrongValuesindex])
                  colnames(wvlistItem) <- c('Index', varNames[i])
                  wrongValuesList[[varNames[i]]] <<- wvlistItem
                }
                if (valid && !analyseAll) abort(message='At least one variable has at least one wrong value',
                                                wrongValuesList=wrongValuesList)
                valid <<- FALSE
              }
            })
            
            if (!valid) abort(message='At least one variable has at least one wrong value',
                              wrongValuesList=wrongValuesList)
            return(TRUE)
          })
