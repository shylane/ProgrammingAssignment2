## a pair of functions that cache the inverse of a matrix.

## Below function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## set initial inverse
  i <- NULL
  
  ## set the matrix
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  
  ## to return the matrix
  get <- function() {
    x
  }
  
  ## to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## to get the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  ## Return matrices and functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Below function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## return inverse if it exists already
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## get inverse
  data <- x$get()
  
  ## solve matrix inversion
  inv <- solve(data, ...)
  
  ## set the inverse matrix
  x$setInverse(inv)
  
  ## returns the matrix inverted
  inv
}
