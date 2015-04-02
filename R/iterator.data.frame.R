##' @importFrom R6 R6Class
data.frame_iterator_generator <- R6::R6Class(
    "data.frame_iterator",
    public = list(
        data = NULL,
        counter = NULL,

        initialize = function(data, ...) {
            self$data <- data
            self$counter <- 1L
        },

        yield = function(j) {
            if (self$is_complete) {
                stop(StopIteration("data.frame is complete"))
            }

            ret <- self$data[self$counter, j]

            self$counter <- self$counter +  1L
            ret
        }),

    active = list(
        is_complete = function(value) {
            self$counter >  self$length
        },

        length = function(value) {
            if (!missing(value)) {
                stop("read only function")
            }
            nrow(self$data)
        }
        )
)

##' @export
iterator.data.frame <- function(object, j,  by.row = TRUE, ...) {
    if (by.row) {
        if (missing(j)) {
            j <- seq_len(ncol(object))
        }
        data.frame_iterator_generator$new(object, j)
    } else {
        if (! missing(j)) {
            warning("j is ignored when iterating over columns")
        }
        basic_iterator_factory$new(object)
    }
}
