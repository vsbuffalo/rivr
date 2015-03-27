## iapply methods

# @export
iapply <- function(x, fun, ...) {
  UseMethod("iapply")
}


#' Iterator apply
#'
#' @param x an iterator object
#' @param fun the function to be applied to each element
#'
#' @param ... optional arguments to \code{fun}
#' @export
iapply.default <- function(x, fun, ...) {
  # TODO: type introspection
  # TODO: x$length is probably not the right thing here - e.g., half
  # complete iterator.
  out <- dynlist()
  i <- 1L
  break_now <- FALSE
  while (not_empty(x)) {
    el <- x$yield()
    out$append(fun(el, ...))
    i <- i + 1L
  }
  out
}


iapply.file_iterator <- function(x, fun, ...) {
  
}


