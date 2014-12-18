## makeCacheMatrix et cacheSolve allow to compute to store in a cache the value of the inverse
## of a iven matrix


## MakeCacheMatrix creates a list of functions on a matrix : get gives the value of the matrix, 
## set changes the value of the matrix and reset the inverse, getinverse gives the value of the
## inverse and setinverse change the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve gives the value of the inverse of a given matrix defined by the function 
## makeCacheMatrix. If this inverse already exists in the cache, it returns it, else it computes it
## and set it to the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
