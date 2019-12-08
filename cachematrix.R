## There's a script which has two functions that calculate 
## and cache the inverse of a matrix.

## Function makeCacheMatrix() creates a special 
## "matrix" object that can cache its inverse.
## We assume that supplied matrix is always invertible
## (it's a square matrix and the determinant of the matrix is nonzero).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) m <<- inv
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}


## Function cacheSolve() computes inverse of the special
## "matrix" returned by makeCacheMatrix. If the inverse has already 
## been calculated (and the matrix has not changed) - the function 
## returns the inverse from the cache.

cacheSolve <- function(x) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("Getting cached data:")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m  
}
