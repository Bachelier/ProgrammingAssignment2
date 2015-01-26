##  In this assignment I coded a pair of functions that cache and compute the inverse of a matrix.
##
## Here is an explanation to both functions:
##
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
## and 2. cacheSolve: This function computes the inverse of the special "matrix" returned by
##
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
##
## changed), then the cachesolve then retrieves the inverse from the cache.


makeCacheMatrix <- function(M = matrix()) {
    inverse <- NULL
    set <- function(x) {
        M <<- x;
        inverse <<- NULL;
    }
    get <- function() return(M);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}##makeCacheMatrix


## This is the second function cacheSolve as explained above.

cacheSolve <- function(M, ...) {
    inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- M$get()
    invserse <- solve(data, ...)
    M$setinv(inverse)
    return(inverse)
}##cacheSolve
