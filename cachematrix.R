## makeCacheMatrix and cacheSolve allow for caching the inverse of a matrix to eliminate redundancy

## makeCacheMatrix sets up the inverse caching for setting and getting of the matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invert) inv <<- invert
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve first checks to see if the inverse is already cached and returns it if so
## otherwise it runs the solve and caches the result for future use

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
