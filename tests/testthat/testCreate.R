context("Creation and characteristics")

test_that("Creating and characteristics 1", {
  x <- big.char(10, 8, init="a")
  names(x) <- letters[1:10]
  y <- rep("a", 10)
  names(y) <- letters[1:10]
  x[1:10] <- letters[11:20]
  y[1:10] <- letters[11:20]
  expect_that(x[], equals(y[]))
  expect_that(is.big.char(x), equals(!is.big.char(y)))
  expect_that(is.character(x), equals(!is.character(y)))
  expect_that(length(x), equals(length(y)))
  expect_that(maxchar(x), equals(8))
  expect_that(names(x), equals(names(y)))
  expect_that(x[letters[1]], equals(y[letters[1]]))
  expect_that(x[letters[1:5]], equals(y[letters[1:5]]))
  expect_that(x[letters[2:5]], equals(y[letters[2:5]]))
})
