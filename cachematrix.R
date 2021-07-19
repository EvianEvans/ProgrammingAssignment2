makeCacheMatrix <- function(x = matrix()) {
         i <- NULL

    set <- function( matrix ) {
            v <<- matrix
            i <<- NULL
    }
    get <- function() {
    	v
    }
    setInverse <- function(inverse) {
        i <<- inverse
    }
    getInverse <- function() {
        i
    }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    v <- x$getInverse()

    if( !is.null(v) ) {
            message("getting cached data")
            return(v)
    }
    data <- x$get()
    v <- solve(data) %*% data
    x$setInverse(v)
    v
}
