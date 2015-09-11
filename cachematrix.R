## These functions (see below) look almost the same as the example for this assignment
## Below these functions there are some commands one can execute in order to check how to use them

## This function takes a matrix as a parameter and creates a special list with four functions: 
## getmatrix, setmatrix, getinverse and setinverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(mtrx) {
        x <<- mtrx
        inv <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(invmtrx) inv <<- invmtrx
    getinverse <- function() inv
    
    list(setmatrix = setmatrix,
         getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function recieves a special list created by makeCacheMatrix function and uses its functions to
## first try to get cached inverse matrix and in case it is NULL the function proceeds to perform calculations

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse of the matrix")
        return(inv)
    }
    mtrx <- x$getmatrix()
    inv <- solve(mtrx, ...)
    x$setinv(inv)
    inv
}

## In order to check how they work one can execute the following commands:
##
## > source("cachematrix.R")
##
## > a <- matrix(c(1, 2, 1, 4, 6, 4, 7, 3, 9), nrow = 3, ncol = 3)
## > b <- makeCacheMatrix(a)
##
## check how "special" list of functions work 
##
## > b$getmatrix()
## [,1] [,2] [,3]
## [1,]    1    4    7
## [2,]    2    6    3
## [3,]    1    4    9
##
## Try getting the cached value directly
## > b$getinverse()
## NULL
## not calculated yet
## 
## make a call to function "cacheSolve" in order to calculate the inverse of a matrix:
## > c <- cacheSolve(b)
## > c
## [,1] [,2]  [,3]
## [1,] -10.50  2.0  7.50
## [2,]   3.75 -0.5 -2.75
## [3,]  -0.50  0.0  0.50
## 
## look if this inverse matrix is now accessible from cache:
## > b$getinverse()
## [,1] [,2]  [,3]
## [1,] -10.50  2.0  7.50
## [2,]   3.75 -0.5 -2.75
## [3,]  -0.50  0.0  0.50
##