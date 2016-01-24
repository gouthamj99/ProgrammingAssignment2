## This file contains funcations that creates a special matrix. 
## This special matrix can be used to cache its inverse. The caching 
## is achieved using <<- operator. 

## This function defines the special matrix. 
## This function is responsible for caching 
## the matrix and the inverse of it. 
makeCacheMatrix <- function(x = matrix()) {

  inverse_matrix <- NULL
  set <- function(input_matrix) {
    #update the matrix
    x <<- input_matrix

    ## reset the inverse to NULL when the matrix is updated
    inverse_matrix <<- NULL
  }
  get <- function() x
  setInverse <- function(input_inverse_matrix) inverse_matrix <<- input_inverse_matrix
  getInverse <- function() inverse_matrix

  ## Return the "special matrix" type. 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function checks if the inverse is already cached. If 'yes'
## then the cached data is returned. Otherwise the inverse is calculated 
## using 'Solve' and the return value of this is cached. 
cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getInverse()

  ## Chcek if the inverse is already cached
  if (!is.null(inverse_matrix)) {
    message("getting cached data")
    return (inverse_matrix)
  }

  ## Inverse is not cached. Inverse should be computed now. 
  original_matrix <- x$get()
  inverse_matrix <- solve(original_matrix, ...)

  ## Cache the inverse that we computed
  x$setInverse(inverse_matrix)

  ## Return the inverse. 
  inverse_matrix
}
