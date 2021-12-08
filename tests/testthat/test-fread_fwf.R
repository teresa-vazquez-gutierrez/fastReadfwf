path <- system.file('extdata', package = 'fastReadfwf')
stSchema <- fastReadfwf::StxlsxToSchema(file.path(path, 'SchemaSNHS.xlsx'), 'stSchema')

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


test_that("special characters read DT", {
  expect_equal(data.DT$Observaciones[nrow(data.DT)], "ñ")
})

test_that("well-parsed DT", {
  expect_equal(dim(data.DT)[2], 51)
})

# test_that("special characters read tbl", {
#   expect_equal(data.tibble[nrow(data.tibble), ncol(data.tibble)], "ñ")
# })

test_that("well-parsed tbl", {
  expect_equal(dim(data.tibble)[2], 51)
})