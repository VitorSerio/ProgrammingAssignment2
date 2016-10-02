## The below functions caches and returns the values of a matrix and it's inverse.


## This function creates a list containing the functions to set the value of the
## matrix, get the value of the matrix, set the value of the inverse of a matrix
## and get the value of the inverse of a matrix.

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(y) {
        m <<- y
        i <<- NULL
    }
    get <- function() m
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function receives a makeCacheMatrix() object type returns the value of
## the inverse of matrix. If the value is cached, it simply returns the cached
## value. Otherwise, the value is calculated.

cacheSolve <- function(m, ...) {
    i <- m$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- m$get()
    i <- solve(data, ...)
    m$setinverse(i)
    i
}
