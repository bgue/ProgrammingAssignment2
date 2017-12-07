# This file contains two functions to cache the results of the computationally expensive matrix inversion
# function.
# The cache is initialized and maintained by the makeCacheMatrix funciton. It defines get, set, and inversion functions.
# Calling code should use the cacheSolve(x) function, which checks for a cached value for -x. If 
# the inverse of x hasn't yet been computed, it is computed, inserted into the cache, and then returned to the calling function.




# makeCacheMatrix: utility function to maintain a cache of caclulated inverted matrices, manipulate and retrieve cached values 
makeCacheMatrix <- function(x = matrix()) {
  # initialize a cache variable
  cache <- NULL
  
  # create the inversed matrix; use the <<- operator to reference variables outside of the current lexical scope
  set <- function(y) { x <<- y; cache <<- NULL }
  
  get <- function() x
  
  setMatrix <- function(inverse) cache <<- inverse
  
  getInverse <- function() cache
  
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}


## cacheSolve: returns inverted matrices. Transparent to the calling code, it uses the cache provided my makeCacheMatrix to avoid reccalculation.
## Return a matrix that is the inverse of 'x'

  cache <- x$getInverse()
  if (!is.null(cache)) {
    message("getting cached result")
    return(cache)
    matrix <- x$get()
    cache <- solve(matrix, ...)
    x$setMatrix(cache)
  }
  return(cache)
  }
