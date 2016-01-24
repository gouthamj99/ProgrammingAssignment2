## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(input_matrix) {
    x <<- input_matrix
    inverse_matrix <<- NULL
  }
  get <- function() x
  setInverse <- function(input_inverse_matrix) inverse_matrix <<- input_inverse_matrix
  getInverse <- function() inverse_matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getInverse()
  if (!is.null(inverse_matrix)) {
    message("getting cached data")
    return (inverse_matrix)
  }
  original_matrix <- x$get()
  inverse_matrix <- solve(original_matrix, ...)
  x$setInverse(inverse_matrix)
  inverse_matrix
}
