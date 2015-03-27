## exception__handler.R -- move to exceptions?

#' @export
empty <- function(x) {
  x$is_complete
}

#' @export
not_empty <- Negate(empty)
