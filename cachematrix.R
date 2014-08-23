## Enable caching of matrix inverse by creating an object
## that stores the inverse and has get/set methods.

## Function to create a "special" matrix object that can
## cache its inverse, providing get, set, getinverse and 
## setinverse methods.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Compute the inverse of the "special" matrix returned by
## makeCacheMatrix(). Return value from cache if already
## calculated and matrix has not changed, otherwise 
## calculate the inverse and store in cache before returning
## the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
