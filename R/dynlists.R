## dynlists.R -- prototype dynamic lists in R
##' @importFrom R6 R6Class

## TODO: Versions of this like vapply where the element type is stored?

dynlist_factory <- R6::R6Class(
  "dynlist",
  public=list(
    data_=NULL,
    length=NULL, # the number of objects
    ## NOTE(RGF): petition to rename size -> capacity to match
    ## stl::vector.
    size=100L, # the allocated size
    rate=9/8,

    ## Instead of accepting ... here we could accept size, rather than
    ## having that be a constant.
    initialize=function(...) {
      self$data_ <- vector("list", self$size)
      self$length <- 0L
      self$append(...)
    },

    ## TODO: Allow preallocation by taking an argument here; so that a
    ## user can say:
    grow=function(to=NULL) {
      #message("zomg!!! growing list")
      if (is.null(to)) {
        nsize <- ceiling(self$size * self$rate)
      } else {
        if (to <= size$size) {
          return()
        } else {
          nsize <- to
        }
      }
      ndata <- vector("list", nsize)
      ndata[seq_len(self$length)] <- self$data_
      self$data_ <- ndata
      self$size <- nsize
    },

    append=function(...) {
      vals <- list(...)
      n <- length(vals)
      if (n == 0L) {
        return()
      }
      idx <- seq_len(n) + self$length
      nlength <- idx[[n]]

      if (nlength > self$size) {
        self$grow()
      }
      self$data_[idx] <- vals
      self$length <- nlength
    },

    get_element=function(i) {
      if (i > x$length) stop("index out of range")
      self$data_[[i]]
    }),
  active=list(
    data=function(value) {
      if (!missing(value)) {
        stop("data is a read-only field")
      }
      self$data_[seq_len(self$length)]
    }
  ))


##' Create dynamically growing list
##' @title Create dynamically growing list
##' @param ... List elements
##' @export
dynlist <- function(...) {
  dynlist_factory$new(...)
}

#' @export
print.dynlist <- function(x, ...) {
  # TODO: make this prettier
  cat(sprintf("dynlist (size: %d, length: %d)\n", x$size, x$length))
  nprint <- min(5L, x$length)
  print(x$data[1:nprint])
  if (x$length > 10L) cat("...\n")
}

#' @export
`[[.dynlist` <- function(i) {
  x$get_element(i)
}

#' @export
as.list.dynlist <- function(x, ...) {
  x$data[seq_len(x$length)]
}

#' @export
length.dynlist <- function(x, ...) {
  x$length
}
