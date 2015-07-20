## The following functions calculate the inverse of a
## square matrix using the 'solve" function. The result
## is cached to avoid the expense of calculating the
## inverse more than once.

## The makeCacheMatrix factory function takes a square
## matrix and returns a cache matrix object that can be
## used to cache the inverse of the matrix

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


## The cacheSolve function takes a cache matrix object
## that contains a square matrix and returns the inverse
## either by calculating it or returning the previously
## calculated cached result

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
