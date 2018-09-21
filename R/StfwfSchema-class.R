#' @title S4 class for the standard data.frame for a fixed-width file schema.
#'
#' @description Definition of the S4 class named \code{StfwfSchema} for the data.frame providing the
#' schema of the fixed-width file to read.
#'
#'
#' @slot Schemadf data.frame with 5 columns:
#'  \itemize{
#'
#'    \item variable: the name of the variable.
#'    \item length: the number of positions which the values of this variable occupies in the file.
#'    \item initialPos: initial position of the field whic hthe values of this variable occupies in
#'    the file.
#'    \item finalPos: final position of the field whic hthe values of this variable occupies in the
#'    file.
#'    \item type: type of the variable. It must be either \code{num} or \code{char}.
#'    \item valueRegEx: regular expression for the values of this variable.
#'    \item description: textual description of the variable.
#'
#'  }
#'
#'
#' @examples
#' # An empty StfwfSchema object:
#' new(Class = 'StfwfSchema')
#'
#' # A trivial example:
#' df <- data.frame(variable = c('Turnover', 'Employees'),
#'                  length = c(9L, 3L),
#'                  initialPos = c(1, 10),
#'                  finalPos = c(9, 12),
#'                  type = rep('num', 2),
#'                  valueRegEx = c('[0-9]{0,9}', '[0-9]{0,3}'),
#'                  description = c('Turnover of the business unit',
#'                                  'Number of employees of the business unit'))
#' new(Class = 'StfwfSchema', df = df)
#'
#'
#' @export
setClass(Class = "StfwfSchema",
         representation = list(df = 'data.frame'),
         prototype = list(df = data.frame(variable = character(0),
                                          length = integer(0),
                                          initialPos = integer(0),
                                          finalPos = integer(0),
                                          type = character(0),
                                          valueRegEx = character(0),
                                          description = character(0))),
         validity = function(object){

           df <- slot(object, 'df')
           if (dim(df)[1] == 0) {

             warning('[StfwfSchema:: validity StfwfSchema] The schema data.frame has 0 rows.\n')

           }

           # Column names
           if (!all(names(df) ==
                    c('variable', 'length', 'initialPos', 'finalPos',
                      'type', 'valueRegEx', 'description'))) {

             stop('[StfwfSchema:: validity StfwfSchema] The schema data.frame has wrong column names. (Check also the order). \n')

           }
           # finPos >= iniPos
           diffPos <- df$finalPos - df$initialPos
           errorDiffPos <- df$variable[diffPos < 0]
           if (length(errorDiffPos) > 0) {

             stop(paste0('[StfwfSchema:: validity StfwfSchema] The following variables have incoherent positions:', paste0(errorDiffPos, collapse = ' , '), '.\n'))

           }
           # length = finPos - iniPos + 1
           diff <- df$finalPos - df$initialPos + 1
           errorLengthVar <- df$variable[diff != df$length]
           if (length(errorLengthVar) > 0) {

             stop(paste0('[StfwfSchema:: validity StfwfSchema] The following variables have incoherent lengths and positions:', paste0(errorLengthVar, collapse = ' , '), '.\n'))

           }

           # sum(length) = finPos[final] - iniPos[initial] + 1
           if (df$finalPos[dim(df)[1]] - df$initialPos[1] + 1 != sum(df$length)) {

             stop('[StfwfSchema:: validity StfwfSchema] The sum of lengths is not coherent with the set of positions.')

          }

          # type num or char
          if (!any(df$type %in% c('num', 'char'))) {

            stop('[StfwfSchema:: validity StfwfSchema] The type must be either char or num.')

          }

          # Is regex?
          notValidRegex <- qdapRegex::is.regex(df$valueRegEx)
          notRegExVar <- df$variable[!notValidRegex]

          if (!all()) {

            stop(paste0('[StfwfSchema:: validity StfwfSchema] The following variables have invalid regex:', paste0(notRegExVar, collapse = ' , '), '.\n'))

          }
          return(TRUE)
         }
)
