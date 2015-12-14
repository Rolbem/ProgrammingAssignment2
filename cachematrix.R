## To avoid recalculating the inverse of a matrix x several times, two functions are defined:
## 1. makeCacheMatrix creates a matrix object inv that can cache the inverse of a matrix
## 2. cacheSolve calculates the inverse and stores it in inv, if it was not calculated before,
##    otherwise (if the inverse was already calculted) it returns the inverse that was stored in inv
## Note that x should be a square, numeric, invertible matrix

## Write a short comment describing this function

## similar to makeVector, makeCacheMatrix initializes inv and reurns a list with 4 functions
## inv is the inverse matrix, which initialy is NULL, and can be calculated with sacheSolve
##  The 4 functions are:
##     1. setmatrix - sets the value of the matrix
##     2. getmatrix - gets the value of the matrix
##     3. setinverse - sets the invere matrix in the matrix inv
##     4. getinverse - retrieves the inverse matrix from inv


makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    getmatrix <- function() x
    
    setinverse <- function(inverse) inv <<- inverse

    getinverse <- function () inv
    
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve first checks whether the inverse of the special "matrix" created with makeCacheMatrix
## already has been stored in inv. If that is not the case (which means that inv is still NULL) then
## it calculates the inverse matrix and stores it in inv for future use. If inv has been calculated 
## before, it gets inv from cache and does not recalculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inv <- x$getinverse()
    if(!is.null(inv)) {
        message ("getting cashed data")
        return (inv)
    }
    data <- x$getmatrix()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
    
}
