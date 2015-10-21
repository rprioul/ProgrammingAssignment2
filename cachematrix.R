## Put comments here that give an overall description of what your
## functions do

## This function creates a list containing a function to: set the value of the matrix, get
## the value of the matrix, set the value of the inverse and get the value of the mean. This
## effectively creates a cache for the operation of getting the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setCacheMatrix <- function(solve) m <<- solve
    getCacheMatrix <- function() m
    list(set = set, get = get,
         setCacheMatrix = setCacheMatrix,
         getCacheMatrix = getCacheMatrix)
}


## This function, similar to the one presented in the assignement, calculates the inverse of
## the matrix created with the precedent function. It first checks in the first lines if it
## has already been calculated and in that cas it returns the cached value. If not, it 
## calculates the inverse of the matrix and sets the matrix found in the cache.

cacheSolve <- function(x, ...) {
    m <- x$getCacheMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setCacheMatrix(m)
    m
}
