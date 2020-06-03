## A special "matrix" object was created which can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  inv <- NULL
  set <- function(y) {
    # use `<<-` to assign a value to an object in an environment 
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  ## Way to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##This new function computes the inverse of the special "matrix" returned above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  # if the inverse has already been calculated
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # otherwise, calculates the inverse 
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
