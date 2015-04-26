## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather that computing it repeatedly.
## The following functions are used to cache the inverse of a matrix.


## makeCacheMatrix will create a list containing a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setMatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}


## The following function return the inverse of the matrix. It first check to see
## if the inverse has already been computed. If it has, it gets the result and skips
## the computation. Otherwise, it will compute the inverse and set the value of the cache
## via setInverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("Retrieving cached data...")
                return(inv)
        }
        cachedData <- x$getMatrix()
        inv <- solve(cachedData)
        x$setInverse(inv)
        inv
}
