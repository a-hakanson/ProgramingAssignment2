## 'makeCacheMatrix' creates a two by two matrix from a vector. 'cacheSolve' inverses
## the matrix that was created from 'makeCacheMatrix'

## The below function sets input 'x' to a two by two matrix, sets the value of 
## the matrix, gets the value of the matrix, sets the value of solve and gets the
## the value of solve

makeCacheMatrix <- function(x) {
  x_sub <- matrix(x, nrow=2, ncol=2)
  s <- NULL
  set <- function(y) {
    x_sub <<- y
    s <<- NULL
  }
  get <- function() x_sub
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The below function inverses the matrix created from the above function. It first 
## checks to see if the matrix has been inversed. If so, it gets the inversion from 
## the chache and skips the computation. Otherwise, it calculates the inversion of 
## the matrix.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
