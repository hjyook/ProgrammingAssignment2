## Put comments here that give an overall description of what your
## functions do


# makeCacheMatrix creates an instance of matrix and sets its matrix and inverse matrix values
# the inverse matrix set to NULL when no input is provided or computed
# cacheSolve either returns the cached inverse matrix or returns the newly computed inverse matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {	
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matdat <- x$get()
  inv<- solve(matdat, ...)
  x$setinv(inv)
  inv
}
