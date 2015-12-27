## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mtx = matrix()) {
    matrix_inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        matrix_inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinverse <- function(inv) matrix_inverse <<- inv;
    getinverse <- function() return(matrix_inverse);
    return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}


## Write a short comment describing this function

cacheSolve <- function(mtx, ...) {
    matrix_inverse <- mtx$getinverse()
    if(!is.null(matrix_inverse)) {
        message("Getting cached data...")
        return(matrix_inverse)
    }
    data <- mtx$get()
    matrix_invserse <- solve(data, ...)
    mtx$setinverse(matrix_inverse)
    return(matrix_inverse)
}
