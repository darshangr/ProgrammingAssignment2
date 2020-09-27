## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  reverse <- NULL
  set <- function(y) {
    x <<- y
    reverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) reverse <<- inverse
  getinverse <- function() reverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  reverse <- x$getinverse()
  if (!is.null(reverse)) {
    message("from cache")
    return(reverse)
  }
  data <- x$get()
  reverse <- solve(data, ...)
  x$setinverse(reverse)
  reverse
}
