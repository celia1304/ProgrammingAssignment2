## The following two functions cache the inverse of a matrix (square/invertible)

## 
## This function creates a special "Matrix" object that can cache its inverse, it expects 
## a matrix as augument. It contains a list of 4 functions: 
## 1. setMatrix => Set the value of input matrix
## 2. getMatrix => Get the value of the cache matrix
## 3. setInverse => Set the value of the inversed matrix 
## 4. getInverse => get the value of the cache inversed matrix
## 
makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    setMatrix <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(solve) {
        invrs <<- solve
    }    
    getInverse <- function() invrs
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

## 
## This function computes the inverse of the special "Matrix" returned by makeCacheMatrix function.
## If the inverse has already been calculated and the matrix has not changed, then it should retrieve
## the inverse from the cache.
##
cachesolve <- function(x, ...) {
    invrs <- x$getInverse()
    if(!is.null(invrs)) {
        message("getting cached data")
        return(invrs)
    }
    data <- x$getMatrix()
    invrs <- solve(data, ...)
    x$setInverse(invrs)
    invrs
}