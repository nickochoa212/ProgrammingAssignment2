## This set of 2 functions allows us to calculate the inverse of 
## a matrix, except that if the matrix has already been calculated
## before, the function pulls the result data from cache instead
## of recalculating it.

## This function creates a list of 4 functions: 1) set the value of
## the matrix 2) get the value of the matrix 3) set the value of the
## inverse 4) get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function calculates the inverse of the special matrix created
## with the makeCacheMatrix function.  If the matrix has been passed
## before, we retrieve the cached value instead of recalculating it.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
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
