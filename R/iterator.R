##' @importFrom R6 R6Class

iterator_generator <- R6::R6Class(
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
        stop("already done")
      }
      ret <- self$data[[self$counter]]
      self$counter <- self$counter + 1L
      ret
    }),
  active=list(
    is_complete=function(value) {
      self$counter > self$length
    },

    length=function(value) {
      if (!missing(value)) {
        stop("read only function")
      }
      length(self$data)
    },

    nremaining=function(value) {
      if (!missing(value)) {
        stop("read only function")
      }
      self$length - self$counter + 1L
    }
  ))


iterator <- function(...) {
  iterator_generator$new(...)
}
## reversed <- function(it) {
## }
