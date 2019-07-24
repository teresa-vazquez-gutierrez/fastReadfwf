#' Show an object of class \linkS4class{StfwfSchema}.
#'
#' \code{show} displays the slot \code{df} of the input \linkS4class{StfwfSchema} object excluding
#' the description column.
#'
#' @param object Object of class \linkS4class{StfwfSchema}.
#'
#' @return Return invisible \code{\link{NULL}}.
#'
#' @examples
#' # A trivial example
#' show(new(Class = 'StfwfSchema'))
#'
#' @include StfwfSchema-class.R getdf.R
#'
#' @export
setMethod(
  f = "show",
  signature(object = "StfwfSchema"),
  function(object){

    output <- getdf(object)
    output$description <- NULL
    methods::show(output)
    invisible(NULL)
  }
)
