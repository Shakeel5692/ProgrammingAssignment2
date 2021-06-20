makeCacheMatrix <- function(x =matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse)  {inv <<- inverse}
  getInverse <- function () {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cachesolve <- function(x,...) {
  inv <- x$getInverse() # function to get the inverse value
  if(!is.null(inv)) {
    message("getting cahced data")
    return(inv)
  }
  mat <- x$get()  # function to get matrix
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}