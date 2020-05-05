## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
##This function creates a special "matrix" object that can cache its inverse
}

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
  inv <- NULL ## iniciates inv as NULL that will hold value of matrix inverse
  set <- function(y){ ## define the set funcyion to assign new
    x <<- y ## the value of matrix in parent environment
    inv <<- NULL ## ih there is a new matrix, reset inv to NULL
  }
  get <- function() x ## define the get function that returns the value of the matrix's argument
  setInverse <- function(inverse) inv <<- inverse ## assigns value of inv in parent invironment
  getInverse <- function() inv ## gets the value of inv where called
  list(set = set, get = get, setInverse = setiInverse, getInverse = getInverse)
  ##Return a list of functions for matrix
}

## Write a short comment describing this function
## This function estimates the inverse of the special matrix returned by makecacheSolve
## If the inverse has already been calculated then cacheSolve will recover the inverse from the cache

cacheSolve <- function(x, ...) {
## This function computes the inverse of the special matrix returned by makeCacheMatrix - return a matrix that is the inverse of 'x'
##If the inverse has already been calculated (and the matrix hasn't changed), then cacheSolve should retrieve the inverse from the cache
  inv <- x$getInverse()
  ##return cached matrix inverse if it's been already computed
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## compute the inverse of matrix
  data <- x$get()
  inv <- solve(data, ...)
  ## cache inverse
  x$setInverse(inv)
  ## return inverse of matric
  inv
}
