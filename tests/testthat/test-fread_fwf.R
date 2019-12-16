path <- system.file('extdata', package = 'fastReadfwf')
stSchema <- fastReadfwf::xlsxToSchema(file.path(path, 'SchemaSNHS.xlsx'), 'stSchema')

# For data.tables
data.DT <- fread_fwf(
  file.path(path, 'MicroDataSNHS.txt'), stSchema, outFormat = 'data.table', perl = TRUE)

# For tibbles
data.tibble <- fread_fwf(
  file.path(path, 'MicroDataSNHS.txt'), stSchema, outFormat = 'tibble')

test_that("class data.table of object read", {
  expect_is(data.DT, "data.table")
})

test_that("class tibble of object read", {
  expect_is(data.tibble, "tbl_df")
})