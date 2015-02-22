## Defines an object which is a cache of x with the inverse set to NULL.
## set() caches a matrix and sets the inverse to NULL
## get() returns the matrix
## setinv() allows a calculated inverse to be stored
## getinv() returns the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Returns the inverse of the matrix.
## If the inverse is already cached, the cached inverse value is returned
## If the inverse has not yet been calculated, the inverse is calculated
## and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("retrieving cached data")
                return(m)
        }
        mdata <- x$get()
        inv <- solve(mdata)
        x$setinv(inv)
        inv    
}