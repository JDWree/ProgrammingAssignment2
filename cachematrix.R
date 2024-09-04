# This script is part of the second assignment for the Johns Hopkins University's Coursera course 'R programming'. 
# Goal is to create functions to solve the inverse of an inversable matrix, but save and read it from cache if possible.

# makeCacheMatrix is to create a "special" matrix object (as a list) that can store its inverse matrix too.
# - set(x), (re)set the data (matrix) / overwriting the object's matrix
# - get(), gets the matrix data of the object
# - setInverse(inv), overwrite the object's inverse matrix
# - getInverse(), gets the inverse matrix data of the object
# It returns a list with the above functions

makeCacheMatrix <- function(x = matrix()) {

    x_inverse <- NULL

    set <- function(y) {
        x <<- y
        x_inverse <<- NULL
    }

    get <- function() {
        x
    }

    setInverse <- function(inv) {
        x_inverse <<- inv
    } 

    getInverse <- function() {
        x_inverse
    }

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse
    )
}


# cacheSolve solves the inverse of the inversable matrix data of a makeCacheMatrix object, store it (into cache) and return it
# If it was already solved before, it will return it from cache.
# !! If a non-inversable matrix is used, this function will fail.

cacheSolve <- function(x, ...) {

    inv <- x$getInverse()

    if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data,...)
    x$setInverse(inv)
    inv
}
