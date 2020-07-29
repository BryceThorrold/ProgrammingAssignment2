## This function will create the list vector the first time -  a list of named of functions.
## stored in a variable that you can use outside of the immediate environment
## You can use $set afterwards to change the original matrix
## Note you have to run it the first time just to get the list setup and available for later reference

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverted) m <<- inverted
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }

  
## We then use cachesolve to take a matrix, check if its inverse is already cached - if it is cached then return 
## that cache, otherwise calculate the new cache, place it into 'm' and print it  

cacheSolve <- function(x, ...) {
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
