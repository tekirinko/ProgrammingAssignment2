## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## Set the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ## Get the matrix
  get <- function() x
  ## Method to set the inverse matrix
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  ## Method to get the inverse matrix
  getInverse <- function() inv
  ## Showing the list of methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  ## Gets the matrix, inverse to x
  inv <- x$getInverse()
  ## Getting off the inversion
  if(!is.null(inv)){
    message("Getting cached data.")
    return(inv)
  }
  ## Get the matrix
  data <- x$get()
  ## Using the matrix multiplication
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}