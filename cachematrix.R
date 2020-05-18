## This file contains functions to create a wrapper around a matrix that enables
## caching the inverse matrix so it can only be computed once

## makeCacheMatrix(): initialized with a matrix object, the matrix object is
## stored and returns a list of four functions: set() for setting (changing)
## the matrix, get() for getting the matrix, setinverse() for storing the
## inverse of the matrix and getinverse() for getting the inverse of the matrix.
## note: getinverse() and setinverse() are not meant to be accessed directly
## but instead cacheSolve(cm) should be used to get the inverse which may cached

makeCacheMatrix <- function(stored_matrix = matrix()) {
   stored_inverse <- NULL
   set <- function(x) {
      stored_matrix <<- x
      stored_inverse <<- NULL
   }
   get <- function() stored_matrix
   setinverse <- function(inverse) stored_inverse <<- inverse
   getinverse <- function() stored_inverse
   list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve(): provides the same functionality for a CacheMatrix object that
## solve() provides for a regular matrix, however, the first time it is called
## the inverse is stored and for subsequent calls the cached value is returned
## instead of computing the inverse anew

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv <- x$getinverse()
   if (!is.null(inv)){
      #stored value available
      message("getting cached data")
      return(inv)
   }
   #else, we have to calculate the inverse
   mx <- x$get()
   inv <- solve(mx)
   x$setinverse(inv)
   inv
}