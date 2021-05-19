path <- system.file('extdata', package = 'fastReadfwf')
stSchema <- fastReadfwf::StxlsxToSchema(file.path(path, 'SchemaSNHS.xlsx'), 'stSchema')

# For data.tables
data.DT <- fread_fwf(
  file.path(path, 'MicroDataSNHS.txt'), stSchema, outFormat = 'data.table', perl = TRUE)

data.DT$newVar <- ""

test_that("Write variable not in schema", {
  expect_error(fwrite_fwf(data.DT, file.path(getwd(), 'MicroDataSNHS'), stSchema, justify = 'right'))
})