## https://github.com/xiangxing98/ProgrammingAssignment2.git
## 1st commit SHA-1 hash identifier: b05b5096e135ae3ae46020952cc914cbce168841
## R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix

## The makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the special "matrix"
## that is returned by makeCacheMatrix.
##cacheSolve checks to see if the inverse has previously been caclculated.
##If it has been calculated and the matrix did not change, then cacheSolve 
##retrieves the inverse from the cache.
##If it was not solved, it calculates the inverse of the matrix and sets 
##the value through the usage of the setinverse function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}