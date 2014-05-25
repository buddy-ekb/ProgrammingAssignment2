## This R file contains a couple of functions that cache the inverse of a matrix
## 1. makeCacheMatrix takes a single matrix parameter and returns a list of
##    four functions to get and set a matrix and its inverse
## 2. cacheSolve takes a single parameter - a result of makeCacheMatrix call and
##    returns the inverse of a matrix trying not to solve it if it was already
##    done

## Creates a list of functions with the environment (scope)
## to maintain a matrix and its (cached) inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
	set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solved) inverse <<- solved
        getinverse <- function() inverse
        list(
    		set = set,
    		get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}

## Checks if a matrix from makeCacheMatrix was already solved;
## if it was then returns a cached version
## otherwise tries to solve a matrix, store the result in the cache
## and then returns it

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'

        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setinverse(inverse)
        inverse
}
