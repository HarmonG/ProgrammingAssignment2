## These functions create a special object that stores a matrix and caches it's inverse.

## This function creates a "special matrix", which is really a list
## containing functions that set the value of the matrix, get the value of the matrix,
## set the value of the inverse and get the value of the inverse

makeCacheMatrix <- function(x = matrix() ) {
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


## This function calculates the inverse of the special matrix created above.
## First, it checks to see if this has already been done.  If so, it gets the inverse. 
## from the cache and skips the computation.  Otherwise, it calculates the inverse.

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