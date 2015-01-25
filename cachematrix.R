## These functions can solve the inverse of a matrix and cache the
## inverse matrix together with original, to save computation time

## This function converts a matrix into a dataframe, which can 
## be used to cache or solve the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
}


## This function takes a dataframe from the above function, and 
## return the inverse of matrix in x

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
