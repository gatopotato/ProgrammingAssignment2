## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to set and get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize a variable to store the inverse matrix (set to NULL initially)
  inv <- NULL
  
  ## Function to set the matrix value
  set <- function(y) {
    x <<- y      ## Set the matrix 'x' to the new value
    inv <<- NULL ## Reset the cached inverse to NULL (since matrix changed)
  }
  
  ## Function to get the matrix value
  get <- function() x
  
  ## Function to set the inverse matrix (cache the result)
  setinverse <- function(inverse) inv <<- inverse
  
  ## Function to get the cached inverse matrix
  getinverse <- function() inv
  
  ## Return a list containing the functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special matrix created by makeCacheMatrix.
## If the inverse has already been calculated, it retrieves the cached result.
## If not, it calculates the inverse, caches it, and returns the result.

cacheSolve <- function(x, ...) {
  
  ## Check if the inverse is already cached
  inv <- x$getinverse()
  
  ## If cached inverse exists, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  ## Otherwise, compute the inverse
  data <- x$get()            ## Get the matrix from the special object
  inv <- solve(data, ...)    ## Calculate the inverse using solve()
  
  ## Cache the computed inverse for future use
  x$setinverse(inv)
  
  ## Return the inverse matrix
  inv
}
