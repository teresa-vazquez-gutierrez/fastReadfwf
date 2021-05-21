#' @title Convert FormatoR to regex.
#' 
#' 
#' @description \code{formatoR2regex} cnverts FormatoR to regex. FormatoR is a
#' specification for a fwf schema according to an internal standard in the 
#' Spanish National Statistics Institute (2020).
#'
#' This standard specifies character, integer and float types by the letters A,
#' I and F, respectively. The width of the string is specified with a number. In
#' the case of float numbers, .d specifies the number of decimal digits after 
#' the decimal point. See example below
#'
#' 
#' @param formatoR character vector with the formatoR specification for the 
#' string widths.
#'
#' @return Return a character vector with regular expressions for each component
#' of the input vector.
#' 
#' @examples 
#' formatoR <- c('A2', 'I5', 'F5.2')
#' formatoR2regex(formatoR)
#' 
#' 
#' @export
formatoR2regex <- function(formatoR){
  
  types <- gsub("[[:digit:]]", "", formatoR)
  types <- gsub("[[:punct:]]", "", types)
  
  types.regex <- sapply(types, function(x){
    switch(x,
           "A" = "[a-zA-Z0-9]", 
           "I" = "[0-9]",
           "F" = "[0-9]",
           "[.]*")
  })
  
  widths <- gsub("[[:alpha:]]", "", formatoR)
  widths <- strsplit(widths, ".", fixed = TRUE)
  
  widths.regex <- sapply(widths, function(x){
    if(length(x) == 0){output <- ""}
    if(length(x) == 1){
      output <- paste0("{1,", x, "}")
    }
    if(length(x) == 2){
      a <- as.integer(x[1]) - as.integer(x[2])
      output <- paste0("{1,", a, "}", "\\.[0-9]{1,", x[2], "}")
    }
    if(length(x) > 2){stop("[fastReadfwf:: formatoR2regex] There exists an element in formatoR of unknown syntax.")}
    return(output)
  })
  
  output <- paste0(types.regex, widths.regex)
  
  return(output)
  
}