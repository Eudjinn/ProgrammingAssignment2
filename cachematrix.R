## These functions (see below) look almost the same as the example for this assignment

## This function takes a matrix as a parameter and creates a special list with four functions: 
## getmatrix, setmatrix, getinverse and setinverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(mtrx) {
        # assigning new matrix to a variable in the upper environment
        x <<- mtrx
        # invalidating cache
        inv <<- NULL
    }
    getmatrix <- function() x
    
    # function that saves an inverse of a matrix in the upper environment 
    setinverse <- function(invmtrx) inv <<- invmtrx
    getinverse <- function() inv
    
    # returning a special object
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
    # if inv is NULL that means either function is executed for the first time or 
    # cache was invalidated using $setmatrix and we need to perform calculations on the new matrix
    mtrx <- x$getmatrix()
    inv <- solve(mtrx, ...)
    # save calculated inverse of the matrix in cache
    x$setinv(inv)
    inv
}
