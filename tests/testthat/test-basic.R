context("basic")

test_that("basic usage", {
  obj <- iterator(1:10)
  for (i in 1:10) {
    i <- obj$yield()
  }
  expect_that(obj$yield(), throws_error("already done"))
})
