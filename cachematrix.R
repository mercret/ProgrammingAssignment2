## The makeCacheMatrix function creates a list containing four functions which 
## can get and set a matrix and its inverse.
##
## cacheSolve takes a list created with makeCacheMatrix, and gives the inverse
## of the matrix it represents. It first looks if the inverse is cached, if not 
## it computes and stores the inverse.

## Creates a list representing a matrix which inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
    solv <- NULL
    set <- function(y){
        x <<- y
        solv <<- NULL
    }
    get <- function() x
    setsolve <- function(s) solv <<- s
    getsolve <- function() solv
    list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## Retrieves the cached inverse of a matrix or computes it

cacheSolve <- function(x, ...) {
    solv <- x$getsolve()
    if(!is.null(solv)){
        message("getting cached data")
        return(solv)
    }
    m <- x$get()
    solv <- solve(m, ...)
    x$setsolve(solv)
    solv
}
