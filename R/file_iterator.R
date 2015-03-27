file_iterator_factory <- R6::R6Class(
  c("file_iterator", "iterator"),

  public=list(
    con=NULL,
    length=NA_integer_,

    initialize=function(filename) {
      ## TODO: file should be connection or file and we should just do
      ## the same thing.
      self$con <- file(filename)
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
      ret <- readLines(self$con, n=1)
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

file_iterator <- function(filename) {
  file_iterator_factory$new(filename)
}
