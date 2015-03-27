context("file_iterator")

test_that("basic", {
  filename <- "../../DESCRIPTION"
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
