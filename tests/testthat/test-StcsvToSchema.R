path <- system.file('extdata', package = 'fastReadfwf')
schema <- StcsvToSchema(file.path(path, 'SchemaSNHS_microdataWeb.csv'), header = TRUE)

test_that("class schema from csv", {
  expect_is(schema, "StfwfSchema")
})