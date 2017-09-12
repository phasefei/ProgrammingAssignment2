## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        inver <<- NULL
        x <<- y
    }
    get <- function() x
    setinverse <- function(inverse_matrix) inver <<- inverse_matrix
    getinverse <- function() inver
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inver <- x$getinverse()
    if (!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data, ...)
    x$setinverse(inver)
    inver
}
