context("zip")

test_that("basic stuff", {
  n <- 10
  it1 <- xseq(n)
  it2 <- distribution_iterator(rnorm)
  it <- zip_iterator(it1, it2)

  expect_that(it$is_complete, is_false())
  expect_that(it$length, equals(n))

  set.seed(1)
  res <- vector("list", n)
  for (i in seq_len(n)) {
    res[[i]] <- it$yield()
  }

  set.seed(1)
  cmp1 <- seq_len(n)
  cmp2 <- rnorm(n)

  for (i in seq_len(n)) {
    expect_that(res[[i]], equals(list(cmp1[[i]], cmp2[[i]])))
  }

  expect_that(it$yield(), throws_error("complete"))
  expect_that(it$is_complete, is_true())
})
