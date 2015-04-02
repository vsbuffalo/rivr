## exception__handler.R -- move to exceptions?

##' Determine if stream/iterator is empty
##' @title Determine if stream/iterator is empty
##' @param x Iterator
##' @export
empty <- function(x) {
  x$is_complete
}

not_empty <- Negate(empty)
