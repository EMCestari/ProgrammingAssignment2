## Put comments here that give an overall description of what your
## functions do

## This function creates a list in order to cache the inverse of a matrix

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function checks the cache or calculates the inverse of a matrix.

cacheSolve <- function(m, ...) {
  i <- m$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  i <- solve(data)
  m$setinv(i)
  i
}