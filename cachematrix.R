## This function implements a cached matrix object with the purpose of storing
##	inverse of the matrix.
## this object provides an environment to store and retrieve the input matrix 
##	and its inverse.
## The object is implemented using a list which has named objects set, get, setinverse, getinverse.

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


## cacheSolve
## given a Cache matrix object, this function returns the cached inverse
##	of the matrix if the result was already computed by a previous
##	invocation of this function
## if this function is called for the first time the inverse will be computed
## 	and stored in the cache matrix object before returning to the caller.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached inverse")
        return (inv)
    }
    origmatrix <- x$get()
    inv <- solve(origmatrix, ...)
    x$setinverse(inv)
    inv
}
