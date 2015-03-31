context("dynlist")

test_that("creation -- trivial", {
  obj <- dynlist()
  expect_that(obj, is_a("dynlist"))
  expect_that(obj$size, equals(100L))
  expect_that(obj$length, equals(0L))
  expect_that(length(obj), equals(0L))
  expect_that(obj$data, equals(list()))
  expect_that(as.list(obj), equals(list()))
  ## Actual storage:
  expect_that(obj$data_, equals(vector("list", obj$size)))
})

test_that("creation -- nontrivial", {
  obj <- dynlist(1, "b", 3)
  expect_that(obj$size, equals(100L))
  expect_that(obj$length, equals(3L))
  expect_that(length(obj), equals(3L))
  expect_that(obj$data, equals(list(1, "b", 3)))
  expect_that(as.list(obj), equals(list(1, "b", 3)))
})

test_that("grow", {
  obj <- dynlist()
  for (i in seq_len(obj$size)) {
    obj$append(i)
  }
  expect_that(obj$size, equals(100L))
  expect_that(obj$length, equals(100L))
  obj$append(101)
  expect_that(obj$size, is_more_than(100L))
  expect_that(obj$length, equals(101L))
  expect_that(obj$data, equals(as.list(seq_len(101L))))
})
