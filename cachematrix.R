## makeCacheMatrix creates "class" CacheMatrix, just the same
## makeVector from the example did. It posesses 4 fields, that can be seen next (where * is)


##input: matrix x, size:NxN
##output: "class" with methods listed below.

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(input) inv <<- input
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve tries to find inverted matrix for input (x) in cache, just like in the example. 
## If it fails to find it, it will compute an return the result.

##input: matrix x, size:NxN
##output: inverted matrix, size NxN

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  message("computing...")
  data <- x$get()
  tmp <- solve(data, ...)
  x$setinv(tmp)
  return(tmp)
}