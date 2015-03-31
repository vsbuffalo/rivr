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
##' @param f A function
##' @export
functional_iterator <- function(f, ...) {
  functional_iterator_factory$new(f, ...)
}

##' Distribution iterator
##' @title Distribution iterator
##' @param f A distribution function
##' @param n Number of things to sample. I am so sorry.
##' @export
distribution_iterator <- function(f, n=1, ...) {
  functional_iterator(f, n=n, ...)
}
