## Two Functions to solve Cached Matrix Inversion  
## Useage Examples at the EOF!

## #1 - a list of four function prototype to 
## INPUT: a Matrix
## OUTPUT: a list with four functions
## ## set, get the matrix [set/get]
## and set/get the Inverse matrix [setInv/getInv]
## The later two are available either after being manual set OR
## by using the cacheSolve() function as shown below
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Helper function to compute and set the inverse
## and check whether the inverse has already been computed
## input: a cacheMatrix list
## output: an inverseMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting inverse matrix from cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

## useage examples:

## > myMatrix <- makeCacheMatrix(matrix(10:7,2,2))
## > myMatrix$get() 
##   [,1] [,2]
##   [1,]   10    8
##   [2,]    9    7
## > cacheSolve(myMatrix)
##   [,1] [,2]
##   [1,] -3.5    4
##   [2,]  4.5   -5
## > myMatrix$getInv()
##   [,1] [,2]
##   [1,] -3.5    4
##   [2,]  4.5   -5
## other matrices to try? myMatrix <- makeCacheMatrix(matrix(rnorm(1000000),1000,1000))
##  > stime <- proc.time(); cacheSolve(myMatrix); proc.time()-stime
##   First Time
##   user  system elapsed 
##   2.506   0.020   2.509 
## > stime <- proc.time(); cacheSolve(myMatrix); proc.time()-stime
##   Second Time
##   user  system elapsed 
##   0.152   0.010   0.142
