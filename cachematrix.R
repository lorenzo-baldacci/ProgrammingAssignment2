## These functions will create a special matrix object wich caches its inverse upon its first calculation


## This function creates the CacheMatrix object with its methods

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv_m) inv <<- inv_m
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function will check for cached inverse and if there is not any it will calculate and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        m <- x$get()
        inv <- solve(m, ...)
        x$setinv(inv)
        inv
}
