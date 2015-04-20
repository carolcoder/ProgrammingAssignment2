## These functions are intended to process the calculations of a inverse matrix
## through cached patterns that allow for better memory usage and less required
## computation. The first function - makeCacheMatrix - will create an inversed 
## matrix while the second function - cacheSolve - will cache the solved result.


## 'makeCacheMatrix' receives a matrix arg and calculates the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  coreMatrix <- x
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }  
  get <- function() x
  getInverse <- function() inverseMatrix
  setInverse <- function(inverted) inverseMatrix <<- inverted
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## 'cacheSolve' receives a cached matrix and calculates the inverse
## either in memory or in cache

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  
  if (!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  coreMatrix <- x$get()
  inverseMatrix <- solve(coreMatrix)
  x$setInverse(inverseMatrix)
  inverseMatrix
}

