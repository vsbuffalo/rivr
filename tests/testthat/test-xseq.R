context("xseq")

test_that("basic usage", {
  obj <- xseq(10)
  res <- integer(10)
  for (i in 1:10) {
    res[i] <- obj$yield()
  }
  expect_that(res, equals(1:10))
  expect_that(obj$yield(), throws_error("sequence is complete"))
})


test_that("basic iapply", {
  obj <- xseq(10)
  expect_that(iapply(obj, function(x) 1L + x),
              equals(as.list(2:11)))
  expect_that(obj$yield(),
              throws_error("sequence is complete"))
})

test_that("drain", {
  res <- drain(xseq(10))
  expect_that(res, equals(as.list(1:10)))
})
