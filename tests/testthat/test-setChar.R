a <- rep(1, 10)
b <- rep("a", 10)
c <- c(125:134)
DT <- data.table(a, b, c)
DT.char <- setChar(DT)
test_that("setChar check", {
  expect_is(DT.char$a, "character")
  expect_is(DT.char$c, "character")
})