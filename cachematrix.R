## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a list containing function to set and get a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setminverse <- function(minverse) mi <<- minverse
  getminverse <- function() mi
  list(set = set, get = get,
       setminverse = setminverse,
       getminverse = getminverse)
}


## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  mi <- x$getminverse()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data, ...)
  x$setminverse(mi)
  mi
}
