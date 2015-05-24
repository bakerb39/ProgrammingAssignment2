#Assignment 2: Caching the Inverse of a Matrix

#Matrix inversion is usually a costly computation and there may be some benefit
#to caching the inverse of a matrix rather than computing it repeatedly. 

#makeCacheMatrix: This function creates a special "matrix" object that can cache
#its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned
#by makeCacheMatrix. If the inverse has already been calculated (and the 
#matrix has not changed), then cacheSolve retrieves the inverse from the 
#cache.

#Assumption: The matrix supplied is always invertible.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}