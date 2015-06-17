## The functions creates a cache object of the inverse of a matrix
## supplied by the user. This will save the time required to create the 
## inverse of the matrix if the data has not been changed.

## The function makeCacheMatrix creates a special matrix object that 
## is really a list that has four elements that are functions that do the following:
##
## 1 - set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the inverse of the matrix
## 4 - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function creates the inverse of the matrix object that was used to 
## create the special vector object makeCacheMatrix. It checks first whether
## the inverse has already been calculated and returns that value. Otherwise it
## calculates and stores it on the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
