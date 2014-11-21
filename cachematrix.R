## Functions to cache the inverse of a matrix instead of calculating the inverse repeatedly
## which can be computationally intensive and time-consuming

## The makeCacheMatrix function creates a special "matrix", which is really a list containing
## four functions that set the value of a matrix, get the value of a matrix,
## set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- null
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## The cacheSolve function calculates the inverse of the special "matrix" created above
## It first checks to see if the inverse has already been computed and gets the inverse from the cace
## Otherwise, it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
