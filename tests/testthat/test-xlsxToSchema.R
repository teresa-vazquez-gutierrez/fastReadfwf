path <- system.file('extdata', package = 'fastReadfwf')
schema <- xlsxToSchema(file.path(path, 'SchemaSNHS.xlsx'), 'stSchema')

test_that("class schema from xlsx", {
  expect_is(schema, "StfwfSchema")
})