context("basic")

test_that("basic usage", {
  obj <- iterator(1:10)
  for (i in 1:10) {
    i <- obj$yield()
  }
  expect_that(obj$yield(), throws_error("already done"))
})


test_that("basic iapply", {
  obj <- iterator(1:10)
  expect_that(iapply(obj, function(x) 1L + x), equals(as.list(2:11)))
  expect_that(obj$yield(), throws_error("already done"))
})


