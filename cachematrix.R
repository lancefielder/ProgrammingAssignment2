## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # set the inv value to null
        inv <- NULL
        # set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL # since the matrix has changed
        }
        # to get the value of the matrix
        get <- function() x
        # to set the inverse
        setinv <- function(inv_) inv <<- inv_
        # to get the inverse
        getinv <- function() inv
        # to return a list of all above functions
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
