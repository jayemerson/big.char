% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/big.char.R
\docType{methods}
\name{[,big.char,ANY,ANY,missing-method}
\alias{[,big.char,ANY,ANY,missing-method}
\alias{[,big.char,ANY,ANY,ANY-method}
\alias{[,big.char,ANY,ANY,logical-method}
\alias{[<-,big.char,ANY,ANY-method}
\alias{[,big.char,missing,ANY,ANY-method}
\alias{[,big.char,missing,ANY,logical-method}
\alias{[,big.char,missing,ANY,missing-method}
\alias{[<-,big.char,missing,ANY-method}
\alias{[,big.char,ANY,missing,ANY-method}
\alias{[,big.char,ANY,missing,logical-method}
\alias{[,big.char,missing,missing,logical-method}
\alias{[,big.char,missing,missing,ANY-method}
\title{non-recommended [:(ANY, ANY, missing) signature}
\usage{
\S4method{[}{big.char,ANY,ANY,missing}(x, i, j, ..., drop = TRUE)

\S4method{[}{big.char,ANY,ANY,ANY}(x, i, j, ..., drop = TRUE)

\S4method{[}{big.char,ANY,ANY,logical}(x, i, j, ..., drop = TRUE)

\S4method{[}{big.char,ANY,ANY}(x, i, j, ...) <- value

\S4method{[}{big.char,missing,ANY,ANY}(x, i, j, ..., drop = TRUE)

\S4method{[}{big.char,missing,ANY,logical}(x, i, j, ..., drop = TRUE)

\S4method{[}{big.char,missing,ANY,missing}(x, i, j, ..., drop = TRUE)

\S4method{[}{big.char,missing,ANY}(x, i, j, ...) <- value

\S4method{[}{big.char,ANY,missing,ANY}(x, i, j, ..., drop = TRUE)

\S4method{[}{big.char,ANY,missing,logical}(x, i, j, ..., drop = TRUE)

\S4method{[}{big.char,missing,missing,logical}(x, i, j, ..., drop = TRUE)

\S4method{[}{big.char,missing,missing,ANY}(x, i, j, ..., drop = TRUE)
}
\arguments{
\item{x}{a \code{\link{big.char}}}

\item{i}{indices (or equivalent) for extraction}

\item{j}{typically not used or supported}

\item{...}{doesn't currently have a role, but may for \code{stringsAsFactors}}

\item{drop}{what you would expect when the returned object is of length 1}

\item{value}{the returned object}
}
