## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse) inv_x <<- inverse
    
    getinv <- function() inv_x
    
    invisible(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

cacheinverse <- function(x) {
    inv_x <- x$getinv()
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    m <- x$get()
    inv_x <- solve(m)
    x$setinv(inv_x)
    inv_x
}


## Write a short comment describing this function

cacheSolve <- function(x) {
    cm <- makeCacheMatrix(x = x)
    cacheinverse(cm)
    cacheinverse(cm)    
}
