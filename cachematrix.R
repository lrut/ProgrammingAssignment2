## Created:    LR
## Date:       2014-10-26

##                                    DESCRIPTION:                              ##

## Matrix inversion is usually a costly computation and there may be some 
##        benefit to caching the inverse of a matrix rather than computing 
##        it repeatedly (there are also alternatives to matrix inversion that 
##        we will not discuss here). 
## This pair of functions caches the inverse of a matrix.



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     ## Default inversed matrix:
     m <- NULL
     
     ## SET the matrix   
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     ## GET the matrix function:
     get <- function() x
     
     ## SET INVERSE of the matrix X function - stored in "m" which is in the global env.:
     setinverse <- function(solve) m <<- solve
     
     ##GET the inversed matrix:
     getinverse <- function() m
     
     ##List the functions:
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
##      makeCacheMatrix above. If the inverse has already been calculated 
##      (and the matrix has not changed), then cacheSolve should retrieve 
##      the inverse from the cache.

cacheSolve <- function(x, ...) {
        
     m <- x$getinverse()
     
     ## If the "M" (inversed matrix) was already calculated then return it from the cache and end the function:
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     ## Get the matrix and calculate the inverse:
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
