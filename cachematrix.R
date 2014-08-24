## The two functions defined here are used to calculate the inverse of a matrix and 
## cache it for quick retrieval. Together these functions make returning the inverse 
## of a complicated matrix much faster after the first use. 

## makeCacheMatrix creates a list of functions that can be called upon
## to calculate the inverse of a matrix "x" and store this value for quick retrieval.
## When using this function, remember to store its output (a list) in a variable (i.e. "z").

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      ## If you use nameOfList$set(nameOfMatrix), this code checks to see if its the same
      ## matrix as previously used to avoid resetting for no reason.
        if (!identical(x, y)){
        x <<- y
        inverse <<- NULL
        }
        else message ("That is the same matrix as last time!")
    }
    get <- function () x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes the list defined from makeCacheMatrix as its argument (i.e. "z"), 
## and will use the functions defined in makeCacheMatrix to return the inverse of the
## matrix stored in makeCacheMatrix.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message ("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
