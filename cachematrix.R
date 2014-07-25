## This file contains the answers to R-Programming assignment 2.
## There are two functions in this file:
##  1) makeCacheMatrix: creates a "matrix" object that cache its inverse.
##  2) cacheSolve: computes the inverse of a given special "matrix" object
##                 returned by makeCacheMatrix(). If the inverse is already
##                 cached, then the cached inverse will be returned.

## This function creates a "matrix" object that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # the functions for setting and getting the matrix and its inverse
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        # return the "matrix" object
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of a given special "matrix" object.
## Return a matrix that is the inverse of 'x'. If the inverse has already
## been cached, the cached inverse will be returned.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        # check if the inverse has already been cached
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        # compute and cache the inverse
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
