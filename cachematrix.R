## These functions compute the inverse of a matrix.  Once the inverse 
## has been calculated, it is stored in the cache, and instead of calculating
## it again, it is fetced from the cache when needed again.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  # Initialize the inverse 
  m <- NULL
  
  # The function setMatrix is used to set the value of the input matrix
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # The function getMatrix is used to fetch the value of the matrix.
  getMatrix <- function() x
  
  # The function setInverse sets the value of the inverse
  setInverse <- function(inverse) m <<- inverse
  
  # The function getInverse fetches the inverse value from the cache
  getInverse <- function() m
  
  # This is what the function makeCacheMatrix returns, a list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
  
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Attempt to fetch the stored inverse of 'x'
  m <- x$getInverse()

  
  # If the inverse exists, we fetch it and return it
  if(!is.null(m)) {
    message("Getting cached inverse")
    return(m)
  }
  
  # Since the inverse doesn't exist yet, we calculate it and store it
  # Begin by fetching the matrix
  data <- x$getMatrix()
  
  # Calculate the inverse
  m <- solve(data)
  
  # Store the value of the inverse
  x$setInverse(m)
  
  # Return the inverse of 'x'
  m
  
}
