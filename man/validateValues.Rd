% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validateValues.R
\name{validateValues}
\alias{validateValues}
\alias{validateValues,data.frame,StfwfSchema-method}
\title{Fast read a fixed-width file}
\usage{
validateValues(
  object,
  StfwfSchema,
  file = "",
  id_var = NULL,
  analyseAll = TRUE,
  perl = FALSE
)

\S4method{validateValues}{data.frame,StfwfSchema}(
  object,
  StfwfSchema,
  file = "",
  id_var = NULL,
  analyseAll = TRUE,
  perl = FALSE
)
}
\arguments{
\item{object}{Object with rectangular shape whose columns will be validated.}

\item{StfwfSchema}{Object of class \linkS4class{StfwfSchema} with the schema of object.}

\item{file}{File to print output to with default value \code{""}.}

\item{id_var}{Data frame with variables to identify observations with wrong values,
with default value \code{NULL}. If not defined, index is used instead.}

\item{analyseAll}{Logical with default value \code{FALSE} to indicate whether to check
all variables with invalid values or not (stopping at the first variable with some invalid value)}

\item{perl}{Logical vector of length 1 with default value \code{FALSE} to indicate whether to use
perl or not in the application of regexp.}
}
\value{
Returns \code{TRUE}.
}
\description{
\code{fread_fwf} takes as basic input a fixed-width filename and its schema and
wraps around \code{\link[data.table]{fread}} from package \linkS4class{data.table} to provide the
contents of the file.

This method is indeed a wrapper for the function \code{\link[data.table]{fread}} followed by the
application of \code{\link[stringi]{stri_sub}}.
}
\examples{
path <- system.file('extdata', package = 'fastReadfwf')
stSchema <- fastReadfwf::StxlsxToSchema(
   file.path(path, 'SchemaSNHS.xlsx'),
   sheetToRead = 'stSchema')

# For data.tables
data <- fread_fwf(
file.path(path, 'MicroDataSNHS.txt'), stSchema, outFormat = 'data.table', perl = TRUE)
validateValues(data, stSchema, perl = TRUE)

# For tibbles
data <- fread_fwf(
file.path(path, 'MicroDataSNHS.txt'), stSchema, outFormat = 'tibble')
validateValues(data, stSchema, perl = TRUE)


}
