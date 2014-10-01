#
# Development for package big.char
#
# Jay Emerson
# October 2014
#
#
# I'll store strings in columns of a big.matrix
# of type char.
#
# FYI -128 is the NA code for char; I support basic ASCII
# only at the moment, codes 0 to 127.  A value of NA
# is dropped and (tentatively) used for padding short
# strings, so I support of strings of shorter than
# the maximum length.
#
# Use 0 for the empty string "", which is different from NA.
# Note the difficulty with charToRaw() and rawToChar().
#
################################################################################
########################################################################### 80 #
####################################################### 60 #

#
# We inherit much of what we need from bigmemory's big.matrix,
# though we also block some functionality of signatures.
# In fact, we probably need to block more than we do at
# the moment, so a few odd thing are probably possible.
#
# The child class big.char inherets behavior from the parent
# class big.matrix (which is called the superclass).
#

#' S4 class big.char inheriting from bigmemory::big.matrix
#' @exportClass big.char
setClass('big.char', contains='big.matrix') 

#' @title Create a big.char object
#'
#' @description
#' Create a \code{big.char} vector of strings
#'
#' @details
#' This is the full set of details for documentation.
#'  
#' @param length the vector length
#' @param maxchar the maximum length of the strings, 8 by default
#' @param init an optional string for initialization purposes
#' @param names optional names, which would be dangerous for long vectors
#' @param backingfile optional name of binary memory-mapped file
#' @param descriptorfile the descriptor file associated with the backingfile
#' @param backingpath should be obvious, right?
#' @param binarydescriptor see \code{\link[bigmemory]{big.matrix}}
#' @param shared see \code{\link[bigmemory]{big.matrix}}
#' @return Returns a \code{big.vector} object
#' @references None.
#' @author Jay Emerson
#' @seealso \code{\link[bigmemory]{big.matrix}}
#' @examples
#' x <- big.char(5, 3, init="ABC")
#' x[]
#' x[1] <- ""
#' 
#' # The following triggers a warning because of the truncation:
#' x[-1] <- c(NA, "*", "--", "DEFG")
#' x[]
#' @keywords datasets
#' @export
big.char <- function(length, maxchar=8,
                     init=NULL, names=NULL,
                     backingfile=NULL,
                     backingpath=NULL,
                     descriptorfile=NULL,
                     binarydescriptor=FALSE,
                     shared=TRUE)
{
  if (!is.null(init)) {
    if (class(init) != "character" || nchar(init) > maxchar ||
          length(init) > 1) 
      stop("Invalid initialization.")
  }
  if (!is.null(names) && length(names) != length) stop("Wrong length names!")
  # RE names: Note that I'm not enforcing things like uniqueness, etc...
  # that really could be important at some point.  Or perhaps
  # this checking is inherited from big.matrix?  I'd need to look.
  
  dimnames <- NULL
  if (!is.null(names)) dimnames <- list(NULL, names)
  x <- bigmemory::big.matrix(nrow=maxchar, ncol=length, type="char",
                             init=NULL, dimnames=dimnames,
                             backingfile=backingfile,
                             backingpath=backingpath,
                             descriptorfile=descriptorfile,
                             binarydescriptor=binarydescriptor,
                             shared=TRUE)
  
  y <- new("big.char", x)
  if (is.null(y))
    stop("Error encountered when creating instance of type big.char")
  if (!is.null(init)) y[] <- init
  return(y)
}

#' @title Attach a shared and/or filebacked big.char object
#'
#' @description
#' The expected usage is for shared-memory parallel computing
#' or for persistence of large \code{\link{big.char}} objects.
#' 
#' @details
#' For details on this sort of attachment, see
#' \code{\link[bigmemory]{big.matrix}}
#' @param obj a descriptor object or file; see \code{\link[bigmemory]{attach.big.matrix}}
#' @param ... other arguments passed through to \code{\link[bigmemory]{attach.big.matrix}}
#' @export
attach.big.char <- function(obj, ...) {
  x <- bigmemory::attach.big.matrix(obj, ...)
  return(new("big.char", x))
}

#' @title Generic function is.big.char()
#' @description Do we have a \code{\link{big.char}}?
#' @details No further detail is needed.
#' @param x a \code{\link{big.char}} vector of strings
#' @export
setGeneric('is.big.char', function(x) standardGeneric('is.big.char'))

#' @title Do we have a big.char?
#' @rdname big.char-methods
#' @exportMethod is.big.char
setMethod('is.big.char', signature(x='big.char'),
          function(x) return(TRUE))

#' @rdname big.char-methods
#' @exportMethod is.big.char
setMethod('is.big.char', definition=function(x) return(FALSE))

#' @title Get the length of a big.char vector
#' @rdname big.char-methods
#' @exportMethod length
setMethod('length', signature(x="big.char"),
          function(x) return(ncol(x)))

#' @title Get the maximum character length of a big.char
#' @param x a \code{\link{big.char}} vector of strings
#' @export
maxchar <- function(x) {
  if (!is.big.char(x)) stop("Not a big.char object.")
  return(nrow(x)) # Remember, strings are stored in columns!
}

#' @title Get the names of a big.char vector
#' @rdname big.char-methods
#' @exportMethod names
setMethod('names', signature(x="big.char"),
          function(x) return(colnames(x)))

#' @title Set the names of a big.char vector
#' @rdname big.char-methods
#' @exportMethod names<-
setMethod('names<-', signature(x="big.char", value="character"),
          function(x, value) {
            warning("Descriptor file (if applicable) is not modified.\n")
            colnames(x) <- value
            return(x)
          })

#
# Now do the get/set signatures for subsetting and assignment.
# We don't want to allow inheritance of some big.matrix
# signatures, so we explicitly block these.  Only a few
# are really needed.  Note that we don't every need
# the drop argument because from the user's perspective
# everything is a vector.
#

### JAY: change drop="missing" to "ANY", or else have empty
### signatures, perhaps, preventing the use of drop which
### serves no purpose here.

#######################################
# (ANY, ANY) signatures; debugging only

#' @title non-recommended [:(ANY, ANY, missing) signature
#' @param x a \code{\link{big.char}}
#' @param i indices (or equivalent) for extraction
#' @param j typically not used or supported
#' @param drop what you would expect when the returned object is of length 1
#' @param ... doesn't currently have a role, but may for \code{stringsAsFactors}
#' @rdname big.char-methods-nonrec
setMethod("[",
          signature(x = "big.char", i="ANY", j="ANY", drop="missing"),
          function(x, i, j, ..., drop) {
            warning(paste("For debugging only: GETTING:",
                          "*value* stored in string j position i"))
            return(bigmemory:::GetElements.bm(x, i, j))
          })

#' @rdname big.char-methods-nonrec
setMethod("[",
          signature(x = "big.char", i="ANY", j="ANY", drop="ANY"),
          function(x, i, j, ..., drop) {
            stop("drop= is not supported or necessary")
          })

#' @title non-recommended [<-:(ANY, ANY) signature
#' @param value the returned object
#' @rdname big.char-methods-nonrec
setMethod('[<-',
          signature(x = "big.char", i="ANY", j="ANY"),
          function(x, i, j, ..., value) {
            if (is.character(value))
              stop("Can't do this set with character; use numeric")
            warning("For debugging only: SETTING *value* for string j position i")
            return(bigmemory:::SetElements.bm(x, i, j, value))
          })

##########################################
# (missing, ANY) signatures; blocked usage

#' @title non-recommended [:(missing, ANY) signature
#' @rdname big.char-methods-nonrec
setMethod("[",
          signature(x = "big.char", i="missing", j="ANY", drop="missing"),
          function(x, i, j, ..., drop) {
            stop("Don't allow manual get:(missing, ANY, missing)")
          })

#' @title non-recommended [<-:(missing, ANY) signature
#' @rdname big.char-methods-nonrec
setMethod('[<-',
          signature(x = "big.char", i="missing", j="ANY"),
          function(x, i, j, ..., value) {
            stop("Don't allow manual set:(missing, ANY)")
          })

##################################################
# (ANY, missing) signatures: most of the real work

# Really available to support x[i], not x[i,], so trap this with nargs()

#' @title Core big.char extraction
#' @rdname big.char-methods
#' @param x a \code{\link{big.char}}
#' @param i indices (or equivalent) for extraction
#' @param j typically not used or supported
#' @param drop what you would expect when the returned object is of length 1
#' @param ... doesn't currently have a role, but may for \code{stringsAsFactors}
#' @exportMethod [
setMethod("[",
          signature(x = "big.char", i="ANY", j="missing", drop="missing"),
          function(x, i, j, ..., drop) {
            if (nargs() >= 3) stop("x[i,] signature not permitted")
            val <- bigmemory:::GetCols.bm(x, i, drop=FALSE) # Note: using cols!
            val[!is.na(val)] <- vapply(val[!is.na(val)],
                                       function(x) rawToChar(as.raw(x)), "")
            return(apply(val, 2, function(x)
              ifelse(all(is.na(x)), NA, paste(x[!is.na(x)], collapse=""))))
          })

#
# The following is a substantial amount of work which may not be
# avoidable.  For example, strsplit(NA, "") throws an error, and
# so we need to trap it and handle it specially.  Next,
# strsplit("", "") returns nothing (instead of ""), which is a
# little extra work; and charToRaw("") is also length 0 instead
# of a value equal to 0 (which would be length 1).
#

#' @title Core big.char assignment
#' @rdname big.char-methods
#' @param value the returned object
#' @exportMethod [<-
setMethod('[<-',
          signature(x = "big.char", i="ANY", j="missing"),
          function(x, i, j, ..., value) {
            if (nargs() == 4) stop("x[i,] signature not permitted")
            areNA <- is.na(value)
            value[areNA] <- "X" # Because strsplit(NA, "") is an error
            value <- strsplit(value, "") # Examine strsplit("", "")
            value <- lapply(value,
                            function(a) {
                              if (length(a)==0) return(0)
                              else return(as.integer(sapply(a, charToRaw)))
                            })
            these <- sapply(value, length)
            value[these < maxchar(x)] <-
              lapply(value[these < maxchar(x)],
                     function(a) c(a, rep(NA, maxchar(x)-length(a))))
            
            # POTENTIAL HOMEWORK EXERCISE:
            if (any(these > maxchar(x))) {
              warning("Long string(s) truncated to maxchar characters")
              value[these > maxchar(x)] <- lapply(value[these > maxchar(x)],
                                                  function(a) a[1:maxchar(x)])
            }
            
            # We may have an assignment bug in bigmemory with a 1-column
            # matrix.
            value <- matrix(unlist(value), nrow=maxchar(x)) # needed
            if (any(areNA)) value[,areNA] <- NA
            if (ncol(value)==1) value <- as.vector(value)
            return(bigmemory:::SetCols.bm(x, i, value))
          })

##############################################
# (missing, missing) signatures: could be used
# but could be very very expensive for large
# objects!

#' @title Full big.char extraction
#' @rdname big.char-methods
#' @exportMethod [
setMethod("[",
          signature(x = "big.char", i="missing", j="missing", drop="missing"),
          function(x, i, j, ..., drop) {
            val <- bigmemory:::GetAll.bm(x)
            if (any(!is.na(val)))
              val[!is.na(val)] <- sapply(val[!is.na(val)],
                                         function(x) rawToChar(as.raw(x)))
            if (!is.matrix(val)) val <- matrix(val, ncol=1)
            return(apply(val, 2,
                         function(x) {
                           ifelse(any(!is.na(x)),
                                  paste(x[!is.na(x)], collapse=""), NA)
                         }))
          })

#' @title Full big.char assignment
#' @rdname big.char-methods
#' @exportMethod [<-
setMethod('[<-',
          signature(x = "big.char", i="missing", j="missing"),
          function(x, i, j, ..., value) {
            x[1:length(x)] <- value   # Can this be improved?
            return(x)
            # Unclear if we can do replication inside the C++ bigmemory
            # functions for handling this more efficiently?  Testing
            # needed, take a look at bigmemory.
            #stop("Not yet implemented")
            #return(SetAll.bm(x, value))
          })

##
## Do I want to intercept other things that might be attempted?
## Also check that inheritance doesn't block the usage that I want
## to detect...
##

####################################################### 60 #
########################################################################### 80 #
################################################################################


