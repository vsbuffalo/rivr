## iapply methods

#' Iterator apply
#'
#' @param x an iterator object
#' @param fun the function to be applied to each element
#' @param ... optional arguments to \code{fun}
#' @export
iapply <- function(x, fun, ...) {
  drain(transform_iterator(x, fun, ...))
}
