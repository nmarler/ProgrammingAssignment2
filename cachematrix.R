# Coursera R-Programming - Programming Assignment 2
#
# Matrix inversion is usually a costly computation and their may be some
# benefit to caching the inverse of a matrix rather than compute it repeatedly
# (there are alternatives to matrix inversion that we will not address here).
#
# The assignment is to write a pair of functions that cache the inverse
# of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    # This function creates a special "matrix" object that can cache
    # its inverse. The R function "solve" is used to calculate the inverse
    # 
    # Args:
    #   x: Matrix whose inverse is to be cached. Must be square, invertible
    #
    # Returns:
    #   Special "matrix" object, that is really a list containing functions to
    #       1. set the value of the matrix
    #       2. get the value of the matrix
    #       3. set the value of the inverse
    #       4. get the value of the inverse
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
    # This function computes the inverse of the special "matrix" returned by
    # makeCacheMatrix above. If the inverse has already been calculated (and
    # the matrix has not changed), then the cachesolve retrieves the inverse
    # from the cache.
    #
    # Args:
    #   x: special "matrix" returned by makeCacheMatrix function
    #
    # Returns:
    #   Matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
