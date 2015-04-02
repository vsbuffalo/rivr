context("file_iterator")

test_that("basic", {
  filename <- "test-file.R"
  it <- file_iterator(filename)

  cmp <- readLines(filename)
  for (i in seq_along(cmp)) {
    expect_that(it$yield(), equals(cmp[[i]]))
  }
  expect_that(it$yield(), throws_error("File is complete"))
  expect_that(it$yield(), throws_error("File is complete"))
  rm(it)
  gc()
})

test_that("basic file iapply", {
  filename <- "test-file.R"
  it <- file_iterator(filename)
  # have to use as.list for now;
  ## browser()
  expect_that(iapply(it, identity), equals(as.list(readLines(filename))))
})
