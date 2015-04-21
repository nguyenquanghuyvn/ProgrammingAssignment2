## Two following functions will cache the inverse of a matrix

## makeCacheMatrix is a function to build up a 
# special matrix for caching its inverse ##

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list( set =set, get = get, 
          setinverse = setinverse, getinverse = getinverse)
}

## cachesolve is a function to compute the inverse of special matrix
#returned by makeCacheMatrix, if the inverse matrix is already existed, 
#cachesolve will take the inverse from the cache

cachesolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    ## Return a matix that is the inverse of 'x'    
}