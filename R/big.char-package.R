########################################################################### 80 #

#' Provides very large vectors of character strings
#'
#' This package extends bigmemory's big.matrix objects
#' to support big vectors of character strings.  The only constraint
#' is the user-specified maximum character length which must be
#' satisfied for every string in the vector.  Shorter strings are
#' padded with NAs, consuming (and wasting) space, but this is
#' unavoidable given the use of memory-mapped files.
#' @docType package
#' @name big.char-package
#' @author Jay Emerson
#' @import methods
#' @importClassesFrom bigmemory big.matrix
NULL
