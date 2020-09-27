## Put comments here that give an overall description of what your
## functions do

##This function will create a "special" matrix object that can cache its inverse.
##This "special" matrix is possible because of the <<- operator, which allows us to assign a value
##to an object in an environment that is diff from the current environment 

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


## Function calculates the inverse of the matrix from makeCacheMatrix. First checks
## to see if the matrix has already been calculated and grabs from Cache if so. Otherwise it does the calculate and caches it

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

