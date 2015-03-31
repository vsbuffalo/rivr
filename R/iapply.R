## iapply methods

#' Iterator apply
#'
#' @param x an iterator object
#' @param fun the function to be applied to each element
#'
#' @param ... optional arguments to \code{fun}
#' @export
iapply <- function(x, fun, ...) {
  UseMethod("iapply")
}

#' @export
iapply.default <- function(x, fun, ...) {
  # TODO: type introspection
  # TODO: x$length is probably not the right thing here - e.g., half
  # complete iterator.
  out <- dynlist()
  break_now <- FALSE
  while (TRUE) {
    el <- tryCatch(x$yield(),
                   StopIteration=function(e) break_now <<- TRUE)
    if (break_now) {
      break
    }
    out$append(fun(el, ...))
  }
  as.list(out)
}


#iapply.file_iterator <- function(x, fun, ...) {
#  
#}
