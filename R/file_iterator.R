file_iterator_factory <- R6::R6Class(
  c("file_iterator", "iterator"),

  public=list(
    con=NULL,
    length=NA_integer_,
    page_in=NULL,
    close_on_exit=TRUE,

    initialize=function(filename, page_in=1L) {
      if (is_connection(filename)) {
        con <- filename
        if (isOpen(con)) {
          self$close_on_exit <- FALSE
        }
      } else {
        self$con <- create_connection(filename)
      }
      if (self$close_on_exit) {
        open(self$con)
        reg.finalizer(self,
                      function(e) if (!is.null(self$con)) close(self$con),
                      onexit=TRUE)
      }
      self$page_in <- page_in
    },

    yield=function() {
      if (is.null(self$con)) {
        ## TODO: violates DRY:
        stop(StopIteration("File is complete"))
      }
      ret <- readLines(self$con, n=self$page_in)
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

create_connection <- function(filename) {
  if (grepl("[a-z]+://", filename)) {
    protocol <- sub("://.*$", "", filename)
    switch(protocol,
           http=url,
           ftp=url,
           file=file,
           stop("unknown protocol"))(filename)
  } else {
    file(filename)
  }
}

is_connection <- function(x) {
  inherits(x, "connection")
}
