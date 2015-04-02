##' Drain a stream
##' @title Drain a stream
##' @param x An iterator/stream object
##' @export
drain <- function(x) {
  if (!rivr_trait_length_finite(x)) {
    stop(InfiniteStream())
  }
  if (rivr_trait_length_known(x)) {
    drain.length_known(x)
  } else {
    drain.default(x)
  }
}

## This won't work for current implementations of xseq and data.frame
## because we don't know how long they have to go.
drain.length_known <- function(x) {
  n <- x$remaining()
  out <- vector("list", n)
  for (i in seq_len(n)) {
    out[[i]] <- x$yield()
  }
  out
}

drain.default <- function(x) {
  out <- dynlist()
  break_now <- FALSE
  repeat {
    el <- tryCatch(x$yield(),
                   StopIteration=function(e) break_now <<- TRUE)
    if (break_now) {
      break
    }
    out$append(el)
  }
  as.list(out)
}

##' @export
as.list.iterator <- function(x, ...) {
  drain(x)
}

## Or implement by terminating iterator -> drain?
collect <- function(it, n) {
  ret <- vector("list", n)
  for (i in seq_len(n)) {
    ret[[i]] <- it$yield()
  }
  ret
}
