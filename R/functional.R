functional_iterator_factory <- R6::R6Class(
  "functional_iterator",

  public=list(
    f=NULL,
    length=Inf,
    is_complete=FALSE,

    initialize=function(f, ...) {
      force(f)
      self$f <- function() f(...)
    },

    yield=function() {
      self$f()
    }
  ))

##' Functional iterator
##' @title Functional iterator
##' @param fun A function
##' @param ... Additional arguments to \code{fun}
##' @export
functional_iterator <- function(fun, ...) {
  functional_iterator_factory$new(fun, ...)
}

##' Distribution iterator
##' @title Distribution iterator
##' @param fun A distribution function
##' @param n Number of things to sample. I am so sorry.
##' @param ... Additional arguments to \code{fun}
##' @export
distribution_iterator <- function(fun, n=1, ...) {
  functional_iterator(fun, n=n, ...)
}
