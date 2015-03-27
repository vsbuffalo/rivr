transform_iterator_factory <- R6::R6Class(
	"transform_iterator",

	public = list(
		it = NULL,
		f = NULL,
		length = NA_integer_,
		is_complete = FALSE,

		initialize = function(it, f, ...) {
			self$it <- it
			self$f <- function(x) f(x, ...)
			self$length <- it$length
		},

		yield = function() {
			x <- self$f(self$it$yield())
			self$is_complete <- self$it$is_complete
			x
		}
	))

##' Transform iterator
##'
##' Apply the function f to the elements of it
##' @param it An iterator
##' @param f A function
##' @export
transform_iterator <- function(it, f, ...) {
	transform_iterator_factory$new(it, f, ...)
}
