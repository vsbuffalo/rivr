## Traits of streams/iterators

## At the moment, doing this with S3 dispatch; later, we can work out
## what bits are too slow or should be done with R6.

##' Traits
##' @title Traits
##' @name rivr_traits
##' @param x Object
##' @param ... Additional arguments
##' @rdname rivr_traits
NULL

##' @export
##' @rdname rivr_traits
rivr_trait_length_finite <- function(x, ...) {
  UseMethod("rivr_trait_length_finite")
}
##' @export
rivr_trait_length_finite.default <- function(x, ...) {
  ## !identical(x$length, Inf) --> probably OK
  len <- x$length
  is.null(len) || is.na(len) || len < Inf
}

##' @export
##' @rdname rivr_traits
rivr_trait_length_known <- function(x, ...) {
  UseMethod("rivr_trait_length_known")
}
##' @export
rivr_trait_length_known.default <- function(x, ...) {
  is.function(x$remaining)
}

## Unimplemented but possible
## rivr_trait_indexable <- function(x, ...) {
##   UseMethod("rivr_trait_indexable")
## }
## rivr_trait_indexable.default <- function(x, ...) {
##   FALSE
## }

## rivr_trait_copyable <- function(x, ...) {
##   UseMethod("rivr_trait_copyable")
## }
## rivr_trait_copyable.default <- function(x, ...) {
##   is.function(x$copy)
## }

## rivr_trait_reversable <- function(x, ...) {
##   UseMethod("rivr_trait_reversable")
## }
## rivr_trait_reversable.default <- function(x, ...) {
##   FALSE
## }

## rivr_trait_resetable <- function(x, ...) {
##   UseMethod("rivr_trait_resetable")
## }
## rivr_trait_resetable.default <- function(x, ...) {
##   FALSE
## }

## rivr_trait_const <- function(x, ...) {
##   UseMethod("rivr_trait_const")
## }
## rivr_trait_const.default <- function(x, ...) {
##   FALSE
## }
