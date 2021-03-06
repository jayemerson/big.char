% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/big.char.R
\name{attach.big.char}
\alias{attach.big.char}
\title{Attach a shared and/or filebacked big.char object}
\usage{
attach.big.char(obj, ...)
}
\arguments{
\item{obj}{a descriptor object or file; see \code{\link[bigmemory]{attach.big.matrix}}}

\item{...}{other arguments passed through to \code{\link[bigmemory]{attach.big.matrix}}}
}
\description{
The expected usage is for shared-memory parallel computing
or for persistence of large \code{\link{big.char}} objects.
}
\details{
For details on this sort of attachment, see
\code{\link[bigmemory]{big.matrix}}
}
