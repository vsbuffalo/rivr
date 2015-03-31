##' @importFrom R6 R6Class
basic_iterator_factory <- R6::R6Class(
  "iterator",
  public=list(
    data=NULL,
    counter=NULL,

    initialize=function(data) {
      self$data <- data
      self$counter <- 1L
    },

    yield=function() {
      if (self$is_complete) {
        ## TODO: use classed exceptions to be more python
        stop(StopIteration("iteration complete"))
      }
      ret <- self$data[[self$counter]]
      self$counter <- self$counter + 1L
      ret
    }),

  active=list(
    is_complete=function(value) {
      if (!missing(value)) {
        stop("field is read-only")
      }
      self$counter > self$length
    },

    length=function(value) {
      if (!missing(value)) {
        stop("field is read-only")
      }
      length(self$data)
    }
  ))

##' Construct a general iterator
##' @title Construct a general iterator
##' @param object Any R object
##' @param ... additional parameters
##' @export
iterator <- function(object, ...) {
  UseMethod("iterator")
}

##' @export
iterator.default <- function(object, ...) {
  basic_iterator_factory$new(object)
}
