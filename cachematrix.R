## On the inital run the functions create a matrix, inverse it and cache the result(s).
## On subesequent runs, if the matrix remains unchanged, the inverse will be retrieved
## from cache to save computation time.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## This function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix is unchanged then the inverse is
## retrieved from cache.

cacheSolve <- function(x, ...) {
        
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
