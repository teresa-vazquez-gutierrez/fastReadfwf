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
#'    \item valueRegExp: regular expression for the values of this variable.
#'    \item description: textual description of the variable.
#'
#'  }
#'
#'
#' @examples
#' # An empty ObsErrorSTDMLEParam object:
#'
#'
#' @export
setClass(Class = "StfwfSchema",
         representation = list(df = 'data.frame'),
         prototype = list(df = data.frame(variable = character(0),
                                          length = integer(0),
                                          initialPos = integer(0),
                                          finalPos = integer(0),
                                          valueRegExp = character(0),
                                          description = character(0))),
         validity = function(object){

           df <- slot(object, 'df')
           if (dim(df)[1] == 0) {

             warning('[StfwfSchema:: validity StfwfSchema] The schema data.frame has 0 rows.\n')

           }

           # Column names
           if (!all(names(df) ==
                    c('variable', 'length', 'initialPos',
                      'finalPos', 'valueRegExp', 'description'))) {

             stop('[StfwfSchema:: validity StfwfSchema] The schema data.frame has wrong column names.\n')

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

           return(TRUE)
         }
)
