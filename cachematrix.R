## These functions deal with caching information and 
## then later accessing that info for further calculations.

## This function creates a "matrix" object and 
## caches the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {

 inverse_x <- NULL
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inverse_x <<-inverse
    getinverse <- function() inverse_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function calculates the inverse of the matrix created by mdakeCacheMatrix, if it has 
## not already been calculated. If the inverse has already been calculated,
## then it returns the inverse that's in the cache.

cacheSolve <- function(x, ...) {
        		
    inverse_x <- x$getinverse()
    if (!is.null(inverse_x)) {
        message("inverse already calculated, retrieving from cache")
        return(inverse_x)
    } else {
        inverse_x <- solve(x$get())
        x$setinverse(inverse_x)
        return(inverse_x)
    }
		
}
