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
#' new(Class = 'ObsErrorSTDMLEParam')
#'
#'
#'
#' @export
setClass(Class = "StfwfSchema",
         representation = c(Schemadf = 'data.frame'),
         prototype = data.frame(variable = character(0),
                                length = integer(0),
                                initialPos = integer(0),
                                finalPos = integer(0),
                                valueRegExp = character(0),
                                description = character(0)),
         validity = function(object){

           if (dim(object)[1] == 0) {

             warning('[StfwfSchema:: validity StfwfSchema] The schema data.frame has 0 rows.\n')

           }

           # Column names
           if (!all(names(object) ==
                    c('variable', 'length', 'initialPos',
                      'finalPos', 'valueRegExp', 'description'))) {

             stop('[StfwfSchema:: validity StfwfSchema] The schema data.frame has wrong column names.\n')

           }

           # length = finPos - iniPos + 1

           return(TRUE)
         }
)
