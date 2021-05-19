path <- system.file('extdata', package = 'fastReadfwf')
stSchema <- fastReadfwf::StxlsxToSchema(file.path(path, 'SchemaSNHS.xlsx'), 'stSchema')

# For data.tables
data.DT.char <- fread_fwf(
     file.path(path, 'MicroDataSNHS.txt'), stSchema, validate = FALSE, convert = FALSE,
       outFormat = 'data.table', perl = TRUE)
data.DT.types <- setTypes(data.DT.char, stSchema)

test_that("class data.table of object read", {
  expect_is(data.DT.char$CMD1, "character")
  expect_is(data.DT.types$CMD1, "numeric")
})