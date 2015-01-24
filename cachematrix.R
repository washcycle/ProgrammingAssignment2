## makeCacheMatrix creates a matrix that can cache the 
## inverse after calulating it for the first time.
##
## cacheSolve 

## create a matrix that will cache the inverse of matrix x 
## after the first call to getinv()

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    
    # set the matrix
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    
    # set the matrix
    get <- function() x
    
    # set cached inverse matrix
    setinv <- function(inverse) inv_x <<- inverse
    
    # If inverse of matrix x has not been calculated; calulate it now.  
    # Then save the inverse matrix into inv_x
    # return the inverse matrix
    getinv <- function() {
        if(!is.null(inv_x)) {
            message("getting cached data")
            return(inv_x)
        }
        inv_x <<- solve(x)
    }
    
    # define list of functions available outside the makeCacheMatrix
    invisible(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## create a CacheMatrix object from matrix x then get the inverse matrix of x
## the first call to cm$getinv calculates, caches and returns the inverse matrix
## the second call gets the cached inverse matrix
##
## param x is assumed to be a matrix where the number of 
## rows equals the nubmer of columns

cacheSolve <- function(x) {
    cm <- makeCacheMatrix(x = x)
    cm$getinv()
    cm$getinv()
}
