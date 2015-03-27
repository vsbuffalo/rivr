xseq_factory <- R6::R6Class(
  c("xseq", "iterator"),

  public=list(
    len=NULL,
    current=NULL,

    initialize=function(length) {
      self$len <- length
      self$current <- 1L
    },

    yield=function() {
      if (self$is_complete) {
        stop("sequence is complete")
      }
      ret <- self$current
      self$current <- self$current + 1L
      ret
    },

    reset=function() {
      self$current <- 1L
    }
  ),

  active=list(
    is_complete=function(value) {
      if (!missing(value)) {
        stop("field is read-only")
      }
      self$current > self$len
    },

    length=function(value) {
      if (!missing(value)) {
        stop("field is read-only")
      }
      self$len
    }
  ))


##' Basic sequence iterator
##' @title Basic sequence iterator
##' @param len Length of the sequence
##' @export
xseq <- function(len) {
  xseq_factory$new(len)
}
