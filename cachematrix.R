## makeCacheMatrix creates a object that can store the inverse matrix in memory 
##
## cacheSolve will cache the matrix

## create a matrix with internal cache for the inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    
    # set the matrix, and reset the inv_x variable
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    
    # set the matrix
    get <- function() x
    
    # set cached inverse matrix
    setinv <- function(inverse) inv_x <<- inverse
    
    # return the inverse matrix
    getinv <- function() inv_x
    
    # define list of functions available outside the makeCacheMatrix
    invisible(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## takes a makeCacheMatrix x then gets the inverse matrix
## the first call to caches inverse matrix (no output)
## the second call gets the cached inverse matrix (prints inverse matrix)
##
## function takes a makeCachedMatrix and any solve parameters
cacheSolve <- function(x, ...) {
    
    if(!is.null(x$getinv())) {
        message("getting cached data")
        return(x$getinv())
    }
    
    x$setinv(solve(x$get(), ...))
}
