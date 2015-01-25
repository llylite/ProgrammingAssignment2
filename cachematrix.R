## These functions together solve the inverse of a matrix and cache the inverse matrix together 
## with the original one, to save the long computation time for the solve inverse function

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

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

## cacheSolve calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation. Otherwise, it calculates the inverse
## of the data and sets the value of the inverse in the cache via the setsolve function.


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
