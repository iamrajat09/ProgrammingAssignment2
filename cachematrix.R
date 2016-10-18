## The function named 'makeCacheMatrix' creates a matrix object used to cache
## inverse of our initial matrix

## 'makeCacheMatrix' takes a matrix as and and makes cache matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## 'cacheSolve' solves the inverse of the special matrix we cached in the
## function 'makeCacheMatrix. If inverse is already calculated then it retrieves
## the inverse

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Retrieving cached data...")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
