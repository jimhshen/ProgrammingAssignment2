## makeCacheMatrix returns a list of functions that cacheMatrix uses
## as its input to either compute the inverse or retreive the cached
## inverse of a matrix

## Creates a special "matrix" object that can cache its inverse. Returns
## a list of functions to set the matrix, get the matrix, set the
## inverse, and get the inrverse. The cacheSolve function uses this
## list as its input.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## then cachesolve retrieves the inverse from the cache. Returns the inverse
## of the matrix.

cacheSolve <- function(x = matrix(), ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
