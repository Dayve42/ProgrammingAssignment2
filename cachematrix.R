
# This function creates a matrix and a list of functions to perform the following operations:
# set the matrix value
# get the matrix value
# set the inverse value of the matrix
# get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        m <<- inverse
    }
    getinverse <- function() {
        m
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function calculates the inverse of a matrix. If the inverse has already been calculated
# and cached, it will return the cached copy instead of performing the calculation again

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        return(m)
    }
    temp <- x$get()
    m <- solve(temp)
    x$setinverse(m)
    m
}
