## Makes a special wrapper object to store matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  myX <- x
  myInverse <- NULL
  set <- function(y) {
    myX <<- y
    myInverse <<- NULL
  }
  get <- function() myX
  setInverse <- function(inverse) myInverse <<- inverse
  getInverse <- function() myInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Cached version of the solve function which completes an inverse of a matrix
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m        
}
