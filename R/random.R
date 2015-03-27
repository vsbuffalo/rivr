pick_iterator_factory <- R6::R6Class(
  "pick_iterator",

  public=list(
    it=NULL,
    pr=NULL,
    length=NA_integer_, # length can't be known

    initialize=function(it, pr) {
      self$it <- it
      self$pr <- pr
    },

    yield=function() {
      repeat {
        ret <- it$yield()
        if (runif(1) < self$pr) {
          return(ret)
        }
      }

      ## Untested but possibly better?  Better still if iterators
      ## supported skip(n)
      ## n <- rgeom(1, pr)
      ## for (i in seq_len(n)) {
      ##   it$yield()
      ## }
      ## it$yield()
    }),

  active=list(
    is_complete=function(value) {
      if (!missing(value)) {
        stop("field is read-only")
      }
      self$it$is_complete
    }))

##' Random sampling iterator
##' @title Random sampling iterator
##' @param it Iterator
##' @param pr Probability of sampling each element
##' @export
pick_iterator <- function(it, pr) {
  pick_iterator_factory$new(it, pr)
}
