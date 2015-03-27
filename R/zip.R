zip_iterator_factory <- R6::R6Class(
  c("zip_iterator", "iterator"),

  public=list(
    iterators=NULL,

    initialize=function(...) {
      self$iterators <- list(...)
    },

    yield=function() {
      lapply(self$iterators, function(it) it$yield())
    }),

  active=list(
    is_complete=function(value) {
      if (!missing(value)) {
        stop("field is read-only")
      }
      any(vapply(self$iterators, function(it) it$is_complete, logical(1)))
    },

    length=function(value) {
      ## TODO: filter out infinite value
      min(vapply(self$iterators, function(it) it$length, numeric(1)))
    }))

zip_iterator <- function(...) {
  zip_iterator_factory$new(...)
}
