## The makeCacheMatrix() function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
##
## The cacheSolve() function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cacheSolve() should retrieve the inverse from 
## the cache.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL # fresh inverse calculation required as new matrix data has arrived
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
  
}


## Calculates the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  ## checking if the inverse has been already calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  ## calculate inverse of the matrix
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
  
}
