context("Basic extractions")

test_that("Toy examples 3by1", {
  x <- big.char(3, 1, init="A")
  x[2:3] <- c("B", "C")
  y <- c("A", "B", "C")
  expect_that(x[], equals(y[]))
  expect_that(x[1], equals(y[1]))
  expect_that(x[-1], equals(y[-1]))
  expect_that(x[1:3], equals(y[1:3]))
  expect_that(x[0], equals(y[0]))
  expect_that(x[c(T,T,F)], equals(y[c(T,T,F)]))
  #expect_that(x[TRUE], equals(y[TRUE])) # Odd -- ideally should work
  #expect_that(x[c(1,4)], equals(y[c(1,4)]))
})

test_that("Toy examples 1by3", {
  x <- big.char(1, 3, init="ABCD")
  y <- "ABC"
  expect_that(x[], equals(y[]))
  expect_that(x[1], equals(y[1]))
  expect_that(x[-1], equals(y[-1]))
  expect_that(x[c(1,1)], equals(y[c(1,1)]))
  expect_that(x[0], equals(y[0]))
  expect_that(x[TRUE], equals(y[TRUE]))
  expect_that(x[FALSE], equals(y[FALSE]))
})

test_that("Toy examples 1by1", {
  x <- big.char(1, 1, init="A")
  y <- "A"
  expect_that(x[], equals(y[]))
  expect_that(x[1], equals(y[1]))
  expect_that(x[0], equals(y[0]))
  expect_that(x[-1], equals(y[-1]))
  expect_that(x[c(1,1)], equals(y[c(1,1)]))
  expect_that(x[TRUE], equals(y[TRUE]))
  expect_that(x[FALSE], equals(y[FALSE]))
  names(x) <- "a"
  names(y) <- "a"
  expect_that(x[], equals(y[]))
  expect_that(x[1], equals(y[1]))
  expect_that(x[c(1,1)], equals(y[c(1,1)]))
  expect_that(x[0], equals(y[0]))
  expect_that(x[-1], equals(y[-1]))
  expect_that(x[TRUE], equals(y[TRUE]))
  expect_that(x[FALSE], equals(y[FALSE]))
  expect_that(x["a"], equals(y["a"]))
})

test_that("Toy examples 2", {
  x <- big.char(5, 2, init="AA")
  y <- c("AA", NA, "", "AA", "\t")
  x[2] <- NA
  x[3] <- ""
  x[5] <- "\t"
  expect_that(x[], equals(y[]))
  expect_that(x[2:4], equals(y[2:4]))
  expect_that(x[-c(1,5)], equals(y[-c(1,5)]))
  #expect_that(x[2], equals(y[2]))  # Odd, x[2] is logical
})

