## MakeCacheMatrix takes a matrix as an argument and stores it along with a 
## list of functions for caching. It then returns the list.
## The list is to be used with "cacheSolve()" below.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invert) inv <<- invert
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve takes a matrix/list made with makeCacheMatrix() and returns its
## inverse. If it is the first call with an object, it also stores the invert
## in cache for future calls of cacheSolve with the same object.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting inverse from cache")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
