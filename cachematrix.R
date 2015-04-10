## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 

## makeCacheMatrix
## This function creates the matrix object as well as 
## defining get and set functions.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # define set 
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # define get
  get <- function() x
  
  # define setinverse, sets inverse matrix to parameter par.
  setinverse <- function(par) inverse <<- par
  
  # define getinverse, returns inverse.
  getinverse <- function() inverse
  
  # creates list of get and set.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
## This function calculates the inverse but first checks if the function does not 
## already exist in cache.

cacheSolve <- function(x, ...) {
  
  ## Check if x contains an inverse.
  inverse <- x$getinverse()
  
  ## if inverse does exist the return it.
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## Otherwise if the inverse does not exist then
  ## get the matrix we want the inverse of.
  data <- x$get()
  
  ## Solve for that inverse.
  inverse <- solve(data, ...)
  
  ## Save that inverse to cache.
  x$setinverse(inverse)
  
  ## Return inverse.
  inverse
  
}
