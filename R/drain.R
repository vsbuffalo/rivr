##' Drain a stream
##' @title Drain a stream
##' @param x An iterator/stream object
##' @export
drain <- function(x) {
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
