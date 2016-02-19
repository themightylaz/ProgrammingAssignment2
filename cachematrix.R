## R Programming Assignment 2: Caching the Inverse of a Matrix.

## Create a matrix object that will cache its inverse.

makeCacheMatrix <- function(the_matrix = matrix()) {
    the_inverse <- NULL
    set <- function(my_matrix) {
        the_matrix <<- my_matrix
        the_inverse <<- NULL
    }
    get <- function() {
        return(the_matrix)
    }
    setInverse <- function(my_inverse) {
        the_inverse <<- my_inverse
    }
    getInverse <- function() {
        return(the_inverse)
    }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Compute the inverse of the matrix created by makeCacheMatrix above. 
## If the inverse already exists and the matrix has not been changed, 
## then retrieve the inverse from cache instead.

cacheSolve <- function(the_matrix, ...) {
    the_inverse <- the_matrix$getInverse()
    if (!is.null(the_inverse)) {
        message("Getting cached data")
        return(the_inverse)
    }
    tmp <- the_matrix$get()
    the_inverse <- solve(tmp, ...)
    the_matrix$setInverse(the_inverse)
    return(the_inverse)
}
