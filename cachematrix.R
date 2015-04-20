# This module contains two functions designed to save 
# on computational work load that working by defining 
# a cache matrix "object"


# produces a cache matrix, which is a list with 4 values
# which are functions that get and set a matrix and it's 
# inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# takes a cacheMatrix and returns the inverse matrix
# while updating the cacheMatrix to cache the inverse
# if the inverse has not already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## Tests which define that the cache matrix behaves as expected
# mat <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
# cmat <- makeCacheMatrix(mat)
# cacheSolve(cmat)
# print(cmat$get() == mat)
# print(cmat$getinverse() == matrix(c(-2, 1, 1.5, -0.5), nrow=2, ncol=2))
