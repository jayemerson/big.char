
#
# Examples for testing and debugging
#

if (FALSE) {
  
  options(bigmemory.typecast.warning=FALSE) # do this in onLoad?
  x <- big.char(10, 8)
  is.big.char(x)
  length(x)
  maxchar(x)
  names(x)
  options(bigmemory.allow.dimnames=TRUE)    # but less sure about this...
  # anticipate descriptorfile issues
  names(x) <- letters[1:10]
  names(x)
  
  x[]
  x[1]
  x[] <- "A"
  x[]
  x[-(1:5)]
  
  x[1,1] <- 76   # The letter 'L'
  x[1:3]
  x["a"]
  x[2] <- "ABCDABCD"
  x[3] <- "XYZ"
  x[]
  x[4,] <- "ZZZ"     # Not allowed, should fail
  x[5:6] <- "HELLO"
  x[7:8] <- c("Hello", "world")
  x[1:4]
  x[]
  
  x["b"]
  x[letters[1:3]]
  
  x[7] <- NA
  x[]
  x[8] <- ""
  x[]
  x[] <- ""
  x[]
  x[5] <- NA
  x[]
  
  x <- big.char(3, 6, init="ABCDEF")
  x[]
  x[1] <- "123456-"         # Truncate and issue warning... done
  x[]
  
  if (!file.exists("testdir")) dir.create("testdir")
  x <- big.char(3, 4, init="ABCD",
                backingfile="test.bin", descriptorfile="test.desc",
                backingpath="testdir")
  x[]
  y <- attach.big.char("test.desc", path="testdir")
  y[]
  x[1] <- "J"
  x[]
  y[]
  
  ### CAUTION: be careful which directory is used for this example
  ### I double-project you by if (FALSE)-ing it:
  
  if (FALSE) {
    
    if (!file.exists("/Users/jay/Desktop/Meetup"))
      dir.create("/Users/jay/Desktop/Meetup")
    setwd("/Users/jay/Desktop/Meetup")
    N <- 1000000000 # One billion strings...
    maxlen <- 20    # ... having up to 20 characters each...
    # ... needs about 20 GB, and I have 16 GB RAM.
    x <- big.char(N, maxlen,
                  backingfile="test.bin",
                  descriptorfile="test.desc")
    x
    x[1:10]
    # x[]           # A really really bad idea
    x[1:10] <- "Hello"
    x[5:10] <- "World"
    x[1:11]
    
    x[length(x)] <- "EndPosition"
    x[length(x)-1] <- "---------------------" # oops
    x[length(x)-1] <- "-------------------+"
    x[(length(x)-5):length(x)]
    
    # This should work but is a bad idea.  Why?
    # Time: 351 seconds (about 6 minutes)
    system.time({
      x[1:length(x)] <- "abcdefghij----------"
    })
    
    x[c(1, length(x))]
    
    library(foreach)    # Thanks to...
    library(itertools)  # ... Steve...
    library(doMC)       # ... Weston!
    registerDoMC(4)     # I have four processor cores here...
    # and get shared memory for "free" with
    # bigmemory...
    
    # Time: about 226 seconds.  Why is this interesting?
    # Because the gains are much less than we might have
    # expected: very disk-intensive, not CPU-intensive.
    system.time({
      iter <- isplitIndices(length(x), chunks=1000)
      ans <- foreach(i=iter, .combine=sum) %dopar% {
        x[i] <- paste(sample(letters, 20), collapse="")
        return(length(i))
      }
    })
    
    ans
    x[1]
    x[length(x)]
    
    # Close up R and start over here for a demonstration:
    y <- attach.big.char("test.desc")
    
    x <- unique(y[sample(length(y), 10000)])
    length(x)
    x[1]
    
    length(y)
    y[1]    # Still have y even though x is something different now.
  }
  
}
