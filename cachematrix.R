## Coursera course: R programming (https://class.coursera.org/rprog-008)
## Programming assignment 2 - Caching the inverse of a matrix (week 3)
## Author: Peter Van Hoof

## makeCacheMatrix: 
##      This function creates a special "matrix" object that 
##      can cache its inverse. It is a list containing a function to
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse matrix
##      4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: 
##      This function computes the inverse of the special "matrix" returned 
##      by makeCacheMatrix above. If the inverse has already been calculated
##      (and the matrix has not changed), then the cachesolve should retrieve 
##      the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m        
}