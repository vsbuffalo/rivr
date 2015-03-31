context("drain")

test_that("drain", {
  it <- iterator(1:10)
  expect_that(drain(it), equals(as.list(it$data)))
})
