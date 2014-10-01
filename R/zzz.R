.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\nbig.char is in development, extending the big.matrix to support vectors\nof strings having maximum length specified by the user.\n")
}
.onLoad <- function(libname, pkgname) {
  options(bigmemory.print.warning=TRUE)
  options(bigmemory.typecast.warning=FALSE)
  options(bigmemory.allow.dimnames=TRUE)
}