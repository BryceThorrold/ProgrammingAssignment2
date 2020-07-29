## This function will create the list vector the first time -  a list of named of functions.
## into x list and store x outside of the immediate environment
## You can use x$set afterwards to change the original data set
## Note you have to run it the first time and place the result into 'x'


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

  
## We then use cachesolve to take a matrix, check if its inverse is cached - if it is cached then return 
## that cache, otherwise calculate the cache, return it  

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
