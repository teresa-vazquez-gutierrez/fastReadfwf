path <- system.file('extdata', package = 'fastReadfwf')
stSchema <- fastReadfwf::xlsxToSchema(
   file.path(path, 'SchemaSNHS.xlsx'),
   sheetname = 'stSchema')

# For data.tables
data <- fread_fwf(
file.path(path, 'MicroDataSNHS.txt'), stSchema, outFormat = 'data.table', perl = TRUE)
a <- capture.output(resultVal <- validateValues(data, stSchema, perl = TRUE))

test_that("Validate Values (good)", {
  expect_true(resultVal)
})

data$SEXOa[1:10] <- "3"

test_that("Validate Values (error)", {
  expect_error(a <- capture.output(validateValues(data, stSchema, perl = TRUE)))
})
