## Inverting a matrix can be a computationally expensive task. These functions
## attempt to provide a solution to that problem by caching matrix inverses 
## in the global environment. The functions search for an existing inverse
## there before running the solve function to compute an inverse from scratch.

## This function sets and retrieves the value of the matrix and its inverse.

makeCacheMatrix <- function (x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    invertMatrix <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set,
         get = get,
         invertMatrix = invertMatrix,
         getInverse = getInverse)
}


## This function computes the inverse of the matrix if a cached value does not
## already exist.

cacheSolve <- function (x, ...) {
    m <- x$getInverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$invertMatrix(m)
    m
}