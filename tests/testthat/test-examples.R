context("examples")

test_that("fib", {
  cmp <- c(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377,
           610, 987, 1597, 2584, 4181, 6765)

  f <- fib()
  d <- unlist(collect(f, 20))
  expect_that(d, equals(cmp))

  ## Or:
  drain(zip_iterator(xseq(20), fib()))
})
