## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function provides methods to set and get a matrix,
## and to set and get its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(a) inv <<- a
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## x is a structure of type makeCacheMatrix
## cacheSolve computes the matrix in the structure of x
## and caches its result.
## if the inverse was already computed it uses it.c

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
