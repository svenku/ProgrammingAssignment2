##              Programming Assignment 2 - R programming - Coursera Data Science Specialization
##
##
##  Here we have 2 functions:
##      makeCacheMatrix - creates R object that stores a matrix and its inverse (R:solve)
##      cacheSolve - takes output of makeCacheMatrix to get the inverse of initial matrix or 
##      (on the subsequent run) gets the cached data (thereby saving calculation time)
##
##


## makeCacheMatrix - creates R object that stores a matrix and its inverse (R:solve)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## cacheSolve - takes output of makeCacheMatrix to get the inverse of initial matrix or 
## (on the subsequent run) gets the cached data (thereby saving calculation time)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    
    if(!is.null(m)) {
        message('getting cached data')
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
