## Put comments here that give an overall description of what your
## functions do


# makeCacheMatrix creates an instance of matrix and sets its matrix and inverse matrix values
# the inverse matrix set to NULL when no input is provided or computed

## Write a short comment describing this function
## To create an instance: x<-makeCacheMatrix()
## you can set the matrix by x$set(cbind(c(9,0,1),c(5,5,0), c(0,4,6)) for instance
## you can also set the inverse manually by x$setinv(cbin(c(0.10344828 ,0.01379310,-0.01724138),
## c(-0.10344828 ,0.18620690, 0.01724138), c(0.06896552, -0.12413793, 0.15517241)))
## x$get()
## x$getinv() 

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

# cacheSolve either returns the cached inverse matrix or returns the newly computed inverse matrix
# cacheSolve(x) does the job given that x is properly initiated with a matrix 

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
