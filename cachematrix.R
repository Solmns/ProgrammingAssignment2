## Functions to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {  
  ## Initialize inv 
  inv <- NULL
  
  ## configure matrix
  set <- function( matrix ) {
    mat <<- matrix
    inv <<- NULL
  }
  
  ## create matrix
  get <- function() {
    ## Return matrix
    mat
  }
  
  ## Function to configure inverse matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## inverse matrix
  getInverse <- function() {
    ## Return the inverse property
    inv
  }
  
  ## Return inverse 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## Compute inverse of  matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getInverse()
  
  ## Just return the inverse if set
  if( !is.null(mat) ) {
    message("return cached data")
    return(mat)
  }
  
  ## Get the matrix 
  data <- x$get()
  
  ## Compute inverse 
  mat <- solve(data) %*% data
  
  ## Set inverse to object
  x$setInverse(mat)
  
  ## Return the matrix
  mat
  
}
