## makeCacheMatrix and cacheSolve are complementary functions which 
## work together to reduce the computational cost of solving for the inverse 
## of a matrix. For any given matrix, the inverse is solved once and then cached for 
## subsequent retrieval.

## The function mackCacheMatrix takes in a matrix and returns a list 
## containing the following functions:
## 1. set: stores the matrix 
## 2. get: retrieves the stored matrix
## 3. setInverse: stores the inverse of the matrix
## 4. getInverse: retrieves the stored inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  ##if set is called, the inverse will be set to null ie. reset value 
  ##since the matrix could possibly have changed
  
  set <- function(matrix) {
    x <<- matrix
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inverse <<- inverse
  
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## The function cacheSolve takes in the returned value of the function makeCacheMatrix
## and checks to see if the inverse of the matrix is cached. If the inverse is cached,
## the inverse is returned. However, if the inverse is not cached, cacheSolve will
## solve for an inverse and cache the inverse value for future retrieval

cacheSolve <- function(x, ...) {
  ##checks to see if the inverse is cached
  inverse <- x$getInverse()
  ##if the inverse is cached, it will be returned
  if(!is.null(inverse)) {
    message("getting cached inverse of matrix")
    return(inverse)
  }
  
  ##if the inverse is not cached, retrieve the value of the stored matrix
  data <- x$get()
  
  ##solve for the inverse of the matrix and then cache the result
  inverse <- solve(data)
  x$setInverse(inverse)
  return(inverse)
}

