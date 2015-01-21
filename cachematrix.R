## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #
        #       Function Name:  makeCacheMatrix
        #       Author:         Matthew Hightower
        #       Date:           January 20, 2015
        #       Purpose:        The purpose of this function is to create a matrix object that
        #                       can store its inverse in cache to save on processing requirements.
        #
        matInverse <- NULL
        set <- function(y) {
                x <<- y
                matInverse <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(solve) {
                matInverse <<- solve
        }
        getInverse <- function() {
                matInverse
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        #
        #       Function Name:  cacheSolve
        #       Author:         Matthew Hightower
        #       Date:           January 20, 2015
        #       Purpose:        The purpose of this function is to compute the inverse of the matrix
        #                       returned by the makeCacheMatrix function. If the inverse has already
        #                       been calculated (and the matrix has not changed), then the cacheSolve
        #                       function retrieves the inverse from the cache to save on duplicative and
        #                       possibly lengthy processing requirements.
        #
        matInverse <- x$getInverse()
        if(!is.null(matInverse) & identical(matInverse, x$getInverse())) {
                message("getting cached data")
                return(matInverse)
        }
        data <- x$get()
        matInverse <- solve(data, ...)
        x$setInverse(matInverse)
        matInverse
}
