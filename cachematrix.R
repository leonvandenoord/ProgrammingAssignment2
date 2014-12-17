## These methods together provide a mechanism to cache (and reuse)
## the results of expensive computations.

## This method creates the object which stores the source matrix and
## if/when calculated the inverse. When the matrix is changed/set the
## calculated inverse is whiped.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve method first checks whether or not a cached result
## is available. If availbe this is returned to the caller, alternatively
## the computation (solve) is performed on the matrix and the result 
## is stored in the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  message("returning newly calculated inverse")
  inverse
}

## Testing method to test our implementation.

testCache <- function() {
  message("testing below should be a calculation");
  cache <- makeCacheMatrix(matrix(c(4,2,3,4),nrow=2, ncol=2))
  cacheSolve(cache)
  message("Cache hit should be below")
  cacheSolve(cache)
  cache$set(matrix(c(-2),nrow=1, ncol=1))
  message("New matrix set, fresh value should be returned.")
  cacheSolve(cache);
}
