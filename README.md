big.char
========

Extends bigmemory's big.matrix to support big vectors of character strings

## Description

This package extends bigmemory's big.matrix objects
to support big vectors of character strings.  The only constraint
is the user-specified maximum character length which must be
satisfied for every string in the vector.  Shorter strings are
padded with NAs, consuming (and wasting) space, but this is
unavoidable given the use of memory-mapped files.

## Getting Started

---
> require(devtools)
> install_github('big.char', 'jayemerson')
> library(big.char)
---

## License

The big.char package is licensed under LGPL-3.

## Hey Jay!

---
git status
git commit -am "Short commit message here"
git push origin
---
