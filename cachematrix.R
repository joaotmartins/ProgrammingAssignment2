##  A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## the cached inverse
        i <- NULL
        
        ## Sets the matrix, re-sets inverse cache
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## Gets the matrix
        get <- function() {
                x
        }
        
        ## Sets the inverse of the matrix
        setinv <- function(inv) {
                i <<- inv
        }
        
        ## Gets the inverse of the matrix
        getinv <- function() {
                i
        }
        
        ## return the matrix object
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getinv()
        
        if (!is.null(inverse)) {
                message("Getting cached inverse value")
                return(inverse)
        }
        
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        
        inverse
}
