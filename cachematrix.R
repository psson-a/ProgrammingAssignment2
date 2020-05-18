## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   stored_inverse <- NULL
   set <- function(x) {
      stored_matrix <<- x
      stored_inverse <- NULL
   }
   get <- function() stored_matrix
   setinverse <- function(inverse) stored_inverse <<- inverse
   getinverse <- function() stored_inverse
   list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv <- x$getinverse()
   if (!is.null(inv)){
      message("getting cached data")
      return(inv)
   }
   #else, we have to calculate the inverse
   mx <- x$get()
   inv <- solve(mx)
   x$storeinverse(inv)
   inv
}
# The following function calculates the mean of the special "vector"
# created with the above function. However, it first checks to see if the
# mean has already been calculated. If so, it `get`s the mean from the
# cache and skips the computation. Otherwise, it calculates the mean of
# the data and sets the value of the mean in the cache via the `setmean`
# function.
# 
# cachemean <- function(x, ...) {
#    m <- x$getmean()
#    if(!is.null(m)) {
#       message("getting cached data")
#       return(m)
#    }
#    data <- x$get()
#    m <- mean(data, ...)
#    x$setmean(m)
#    m
# }
