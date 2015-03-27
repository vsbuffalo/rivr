

#' Iterator apply
#'
#' @param x an iterator object
#' @param fun the function to be applied to each element
#'
#' @param ... optional arguments to \code{fun}
#' @export
iapply <- function(x, fun, ...) {
  # TODO: type introspection
  out <- vector("list", length=x$length)
  i <- 1L
  while (!x$is_complete) {
    out[[i]] <- fun(x$yield(), ...)
    i <- i + 1L
  }
  out
}


