## makeCacheMatrix function creates a special "matrix" that can cache its inverse. 
##Thus it sets the matrix when data is provided and get function retrieves the matrix, that is, returns the value of the matrix's argument

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y){ 
    x <<- y 
    inv <<- NULL 
  }
  
  get <- function() x 
  setInverse <- function(inverse) inv <<- inverse 
  getInverse <- function() inv 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve function estimates the inverse of the special matrix returned by makeCacheMatrix, i.e., if the inverse matrix is already computed 
## If it is, it gets the data from makeCacheSolve, i.e., it will recover the inverse from the cache, else it computes the matrix and pushes it to the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
