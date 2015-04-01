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

## Or implement by terminating iterator -> drain?
collect <- function(it, n) {
  ret <- vector("list", n)
  for (i in seq_len(n)) {
    ret[[i]] <- it$yield()
  }
  ret
}
