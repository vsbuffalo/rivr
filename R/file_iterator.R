file_iterator_factory <- R6::R6Class(
  c("file_iterator", "iterator"),

  public=list(
    con=NULL,
    length=NA_integer_,
    n=NULL,

    initialize=function(filename, n=1L) {
      ## TODO: file should be connection or file and we should just do
      ## the same thing.
      self$con <- file(filename)
      self$n <- n
      open(self$con)
      reg.finalizer(self,
                    function(e) if (!is.null(self$con)) close(self$con),
                    onexit=TRUE)
    },

    yield=function() {
      if (is.null(self$con)) {
        ## TODO: violates DRY:
        stop(StopIteration("File is complete"))
      }
      ret <- readLines(self$con, n=self$n)
      if (length(ret) == 0L) {
        close(self$con)
        reg.finalizer(self, function(e) {}, onexit=TRUE)
        self$con <- NULL
        stop(StopIteration("File is complete"))
      }
      ret
    }),
  active=list(
    is_complete=function(value) {
      ## if (!missing(value)) {
      ##   stop("field is read-only")
      ## }
      ## !isIncomplete(self$con)
      stop("not yet implemented")
    }))

##' File iterator
##' @title File iterator
##' @param filename Filename
##' @param n Number of lines to yield each time
##' @export
file_iterator <- function(filename, n=1L) {
  file_iterator_factory$new(filename, n)
}
