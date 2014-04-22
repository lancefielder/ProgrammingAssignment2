## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv_) inv <<- inv_
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


cacheSolve <- function(x, ...) {
        # check if the inverse is already cached
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # not cached, so we get the matrix into data
        data <- x$get()
        # and compute the inverse
        inv <- solve(data, ...)
        # then cache the inverse
        x$setinv(inv)
        # and return it as well
        inv
}
