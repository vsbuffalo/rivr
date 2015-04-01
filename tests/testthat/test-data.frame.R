context("data.frame")

test_that("basic data.frame", {
  d <- mtcars
  it <- iterator(d)

  expect_that(it$length, equals(nrow(d)))
  expect_that(it$is_complete, is_false())
  for (i in seq_len(nrow(d))) {
    expect_that(it$yield(), equals(d[i,]))
  }
  expect_that(it$yield(), throws_error("data.frame is complete"))
})
