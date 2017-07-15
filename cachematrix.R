## Functions for creating a matrix with a cached inverse for
## faster computing, and retrieving the inverse. The inverse 
## is only set on the initial call  so its assumed that 
## the data within the matrix are static.

## Create a matrix that has a cached copy of its inverse.
## Create functions for getting the value of the matrix,
## setting the matrix, getting the value of the inverse,
## and setting the inverse 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse = setinverse, getinverse=getinverse)
}



## Return a matrix that is the inverse of 'x'
## If the inverse has been previously set use
## the cached value otherwise compute the inverse
## and set the cached copy
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return (inv)
    }
    m = x$get()
    inv <- solve(m)
    x$setinverse(inv)
    inv
}
