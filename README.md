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

The next to last command will trigger a warning because the string
is truncated to three (3) characters.

---
    > require(devtools)
    > install_github('big.char', 'jayemerson')
    > library(big.char)
    > x <- big.char(5, 3, init="ABC")
    > x[]
    > x[1] <- ""
    > x[-1] <- c(NA, "*", "--", "ABCD")
---

## License

The big.char package is licensed under LGPL-3.

## Hey Jay!

After creating the public repository on GitHib, essentially empty
with only `README.md`, I cloned to my laptop:

    git clone https://github.com/jayemerson/big.char.git
    
I then moved the package contents into the local `big.char` directory,
at which point the following workflow appears sufficient for my own
purposes:

---
    git status
    git commit -am "Short commit message here"
    git push origin
---

