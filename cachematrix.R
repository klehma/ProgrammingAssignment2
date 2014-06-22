## These functions allow the caching of the inverse of a matrix
## to enable faster retrieval

## makeCacheMatrix creates a special matrix object that is able
## to cache its own inverse

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
  
}


## cacheSolve returns the inverse of the argument matrix, using 
## the cached value if it has already been solved

cacheSolve <- function(x, ...) {
        
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
  
}
