## This would actually be a special case of a lagged iterator?

## 1 1 2 3 5 7 12 19
fib_factory <- R6::R6Class(
  c("fib", "iterator"),
  public=list(
    dat=c(0, 1),
    length=Inf,
    is_complete=FALSE,
    yield=function() {
      x <- self$dat
      ret <- x[[2]]
      self$dat <- c(ret, x[[1]] + x[[2]])
      ret
    }))

fib <- function() {
  fib_factory$new()
}

prime_factory <- R6::R6Class(
  c("prime", "iterator"),
  public=list(
    cur=1,
    length=Inf,
    is_complete=FALSE,
    yield=function() {
      ## sieve of erathsmus
    }))
