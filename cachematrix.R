## Put comments here that give an overall description of what your
## functions do

## Create a matrix that will allow the inverse to be cached

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
            x <<- y
            xinv <<- NULL
    }
    get <- function() x
    setinv <- function(theinv) xinv <<- theinv
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Checks for a cached inverse. If there is none, calculates
## the inverse

cacheSolve <- function(x, ...) {
    xinv <- x$getinv()
    if(!is.null(xinv)) {
            message("getting cached data")
            return(xinv)
    }
    data <- x$get()
    xinv <- solve(data)
    x$setinv(xinv)
    xinv

}
