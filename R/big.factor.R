#
# Provide big.factor on top of big.char and big.matrix
#

#' S4 class big.factor contains a big.matrix (vector) of integers
#' and a big.char of factor levels.
#' exportClass big.factor
setClass('big.factor', representation(val='big.matrix',
                                       levels='big.char'))

#' @title Create a big.factor object
#' 
#' @description Create a \code{big.factor} object that could be
#' in shared memory or larger-than-RAM via \code{\link{big.char}}
#' and \code{\link[bigmemory]{big.matrix}} objects.
#' 
#' @details There are real hazards here.  I may not finish it now.
#' @return a \code{big.factor}
#' @param length the vector length
#' @param maxchar the maximum length of the strings, 8 by default
#' @param init an optional string for initialization purposes
#' @param levels a vector of character factor levels (labels), which may
#' be truncated to \code{maxchar} length if necessary.
#' @param backingfile optional name of binary memory-mapped file
#' @param descriptorfile the descriptor file associated with the backingfile
#' @param backingpath should be obvious, right?
#' @param binarydescriptor see \code{\link[bigmemory]{big.matrix}}
#' @param shared see \code{\link[bigmemory]{big.matrix}}
#' @author Jay Emerson
#' @export
big.factor <- function(length,
                       maxchar=8,
                       init=NULL,
                       levels=NULL,
                       backingfile=NULL,
                       backingpath=NULL,
                       descriptorfile=NULL,
                       binarydescriptor=FALSE,
                       shared=TRUE) {
  
  val <- bigmemory::big.matrix(length, 1, type='integer', init=NULL,
                               backingfile=backingfile,
                               backingpath=backingpath,
                               descriptorfile=descriptorfile,
                               binarydescriptor=binarydescriptor,
                               shared=shared)
  if (!is.null(backingfile)) backingfile <- paste("LEVELS_", backingfile, sep="")
  if (!is.null(descriptorfile)) descriptorfile <- paste("LEVELS_", descriptorfile, sep="")
  lev <- big.char(length(unique(levels)), maxchar=maxchar, init="",
                  backingfile=backingfile,
                  backingpath=backingpath,
                  descriptorfile=descriptorfile,
                  binarydescriptor=binarydescriptor,
                  shared=shared)
  lev[] <- levels
  if (length(init)>1) stop("Simple initialization only")
  if (!is.null(init)) {
    if (is.character(init)) {
      init <- which(levels == init)
    }
    # The following could be made less costly:
    if (!any(init %in% 1:length(levels))) stop("Invalid initialization") 
    val[] <- init
  }
  x <- new('big.factor', val=val, levels=lev)
  
  return(x)
  
}

#' Coerce a vector of character to a big.factor, quick-and-dirty for testing,
#' no frills, etc...
#' @param x a vector of character (for now)
#' @return a \code{\link{big.factor}}
#' @export
as.big.factor <- function(x) {
  levels <- sort(unique(x))
  ans <- big.factor(length(x),
                    maxchar=max(nchar(x)),
                    levels=levels)
  ans@val[] <- match(x, levels)
  return(ans)
}

if (FALSE) {
  
  x <- big.factor(10, init="A", levels=c("A", "B"))
  sim <- sample(letters[1:5], 100, replace=TRUE)
  y <- as.big.factor(sim)
  
}