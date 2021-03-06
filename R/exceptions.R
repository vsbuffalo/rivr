StopIteration <- function(message="Iteration is complete", call=NULL) {
  class <- c("StopIteration", "error", "condition")
  structure(list(message = as.character(message), call = call),
            class = class)
}

InfiniteStream <- function(message="Operation would never terminate",
                           call=NULL) {
  class <- c("InfiniteStream", "error", "condition")
  structure(list(message = as.character(message), call = call),
            class = class)
}
