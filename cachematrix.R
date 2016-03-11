## These functions aim to hel us to cache the inverse of a matrix.
## Developed by Ivette Luna on Friday, 11 of May, 2016
## For R-Programming course at Coursera
## Tip: other operations for matrices:
## http://www.statmethods.net/advstats/matrix.html

## --------------------------------------------------------------------------

## This function creates a special "matrix" object that can cache its inverse
## It's assumed that the matrix is invertible - so, we don't check that
## Sort of object in java

makeCacheMatrix <- function(x = matrix()) {

        inverted <- NULL
        set <- function(y) {
                x <<- y
                inverted <<- NULL
        }
        get <- function() x
        setinverse <- function(invertedOut) inverted <<- invertedOut
        getinverse <- function() inverted
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

} # end makeCacheMatrix




## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
## Sort of proc in Matlab

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  ## x is of'type' makeCacheMatrix


        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse

} # end cacheSolve
