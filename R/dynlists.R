## dynlists.R -- prototype dynamic lists in R
##' @importFrom R6 R6Class


dynlist_factory <- R6::R6Class(
  "dynlist",
  public=list(
    data=NULL,
    length=NULL, # the number of objects
    size=100L, # the allocated size
    rate=9/8,

    initialize=function(...) {
      data <- list(...)
      #size <- ceiling(self$size*self$rate)
      self$data <- vector("list", self$size)
      self$length <- length(data)
    },

    grow=function() {
      #message("zomg!!! growing list")
      nsize <- ceiling(self$size*self$rate)
      ndata <- vector("list", nsize)
      ndata[1:self$length] <- self$data
      self$data <- ndata
      self$size <- nsize
    },

    append=function(...) {
      vals <- list(...)
      n <- length(vals)
      if (n == 0L) stop("append() must have at least one argument")
      if (self$length + n > self$size) self$grow()
      self$data[self$length:(self$length + n)] <- vals
      self$length <- self$length + n
    }

  ))

#' @export
dynlist <- function(...) {
  dynlist_factory$new(...)
}

print.dynlist <- function(x, ...) {
  # TODO: make this prettier
  cat(sprintf("dynlist (size: %d, length: %d)\n", x$size, x$length))
  nprint <- min(5L, x$length)
  print(x$data[1:nprint])
  if (x$length > 10L) cat("...\n")
}







