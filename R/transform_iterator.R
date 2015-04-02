transform_iterator_factory <- R6::R6Class(
	"transform_iterator",

	public = list(
		it = NULL,
		f = NULL,
		## Here, could use active binding functions that pass
                ## to iterator?
		length = NA_integer_,
		is_complete = NA,

		initialize = function(it, f, ...) {
			force(f)
			self$it <- it
			self$f <- function(x) f(x, ...)
		},

		yield = function() {
                  self$f(self$it$yield())
		}
	))

##' Transform iterator
##'
##' Apply the function f to the elements of it
##' @param it An iterator
##' @param fun A function
##' @param ... Additional arguments to \code{fun}
##' @export
transform_iterator <- function(it, fun, ...) {
	transform_iterator_factory$new(it, fun, ...)
}

##' Create lazy pipe
##' @title Create lazy pipe
##' @param it Iterator
##' @param fun Function
##' @rdname lazy_pipe
##' @name lazy_pipe
##' @export
`%|%` <- function(it, fun) {
  transform_iterator(it, fun)
}
