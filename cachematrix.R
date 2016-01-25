## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly
## This function can be used to cach the inverse of a Matrix.

## makeCacheMatrix creates a list containing a function to do the following task:
## a. Set the value of the matrix
## b. Get the value of the Matrix
## c. Set the value of the inverse of the Matrix
## d. Get the value of the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## The following function calculates the inverse of a matrix. 
## Firstly it checks if the inverse is already computed. In that case it gets the result 
## from cache and skips the coputation. If not, it will compute the inverse of the matrix, cache the result and
## returns the data. 

## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data.")
        return(m)
    }
    mat <- x$get()
    m <- solve(mat)
    x$setinverse(m)
    m
}