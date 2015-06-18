## These functions will work together to store a matrix (makeCacheMatrix) and 
## to cache it's inverse (cacheSolve)

## makeCacheMatrix will take a matrix and return a list of functions for setting
## and getting that matix or setting and getting the inverse of the 
## matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will calculate the inverse of the matrix returned by the 
## makeCacheMatrix function if it hasn't been calculated yet, or it will return 
## the cached version if it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
