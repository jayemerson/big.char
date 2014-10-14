big.char
========

Extends bigmemory's big.matrix to support big vectors of character strings.
Current package build status via use of
[Travis CI](https://travis-ci.org/jayemerson/big.char):
<a href="https://travis-ci.org/jayemerson/big.char"><img src="https://travis-ci.org/jayemerson/big.char.svg?branch=master"></a>

## Description

This package extends bigmemory's big.matrix objects
to support big vectors of character strings.  The only constraint
is the user-specified maximum character length which must be
satisfied for every string in the vector.  Shorter strings are
padded with NAs, consuming (and wasting) space, but this is
unavoidable given the use of memory-mapped files.

## NOTE: under development!

This package is very much under development.  The basic functionality
seems fairly solid.  The implementation may not be as efficient as
it could be.  Not all usage errors may be trapped or handled
in a friendly or appropriate manner.
For example, if a `big.char` vector, `x` has length 5, then
`x[6]` generates an error (this would not be the case with a regular
vector of strings).  Please feel free to email me with problems,
questions, or requests.

Known issues to be resolved:
- index recycling (bug in bigmemory, fix there)
- indices out of bounds (needs fixing in big.char for consistency
with R's default behavior/behaviour)

## Getting Started

The next to last command triggers a warning because the string
is truncated to three (3) characters.

---
    > require(devtools)
    > install_github('big.char', 'jayemerson')
    > library(big.char)
    >
    > x <- big.char(5, 3, init="ABC")
    > x[]
    [1] "ABC" "ABC" "ABC" "ABC" "ABC"
    > x[1] <- ""
    > x[-1] <- c(NA, "*", "--", "ABCD")
    Warning message:
    In `[<-`(`*tmp*`, -1, value = c(NA, "*", "--", "DEFG")) :
    Long string(s) truncated to maxchar characters
    > x[]
    [1] ""    NA    "*"   "--"  "DEF"
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

