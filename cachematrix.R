## Matrix inversion is usually costly computation and there may be some benifit to caching the inverse of matrix rather than computing
## it repeatedly This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
    N <- NULL
    set <- function(y) {
        x <<- y
        N <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) N <<- inverse
    getinverse <- function() N
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This second function computes the inverse of the special "matrix" created by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    N <- x$getinverse()
    if(!is.null(N)) {
        message("getting cached data")
        return(N)
    }
    data <- x$get()
    N <- solve(data, ...)
    x$setinverse(N)
    N
}
