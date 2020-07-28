## This function will create the vector the first time, put the list of functions
## into x kist and 'publish'x outside of the immediate environment
## You can use $set afterwards to change the original data set
## Note you have to run it the first time and place the result into 'x'



makeCacheMatrix <- function(current = matrix()) {
    inverse_matrix <- NULL
    set <- function(new) {
      current <<- new
      inverse_matrix <<- NULL
    }
    get <- function() current
    setinverse <- function(inverted) inverse_matrix <<- inverted
    getinverse <- function() inverse_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }

  
## We then use cachesolve to solve for the matrix passed to it, called x

cacheSolve <- function(x, ...) {
  exists <- x$getinverse()
  if(!is.null(exists)) {
    message("getting cached data")
    return(exists)
  }
  data <- x$get()
  inverted <- solve(data, ...)
  x$setinverse(inverted)
  inverted
}
