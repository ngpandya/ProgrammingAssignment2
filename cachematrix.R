## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  
  inver <- x
  set <- function(y) {
    x <<- y
    inver <<- x
  }
  get <- function() x
  setinver <- function(invererse) inver <<- invererse
  final <- function(inver) 
    list(set = set, get = get, setinver = setinver, final = final)
}


cacheSolve <- function(x, ...) {
  
  inver <- x$final()
  if(!is.null(inver)) {
    message("cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinver(inver)
  inver
}
