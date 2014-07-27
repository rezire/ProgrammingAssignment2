## These functions create a framework to store and retrieve matrices and their inverses by eliminating unnecessary inversion
##    calculations.


## This function makes a special "vector", which is really a list containing functions to:
##  1. set the matrix of interest and reset the cached inverse value
##  2. get the matrix of interest
##  3. set the inverse of the matrix of interest
##  4. get the inverse of the matrix of interest

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## This function sets and returns the inverse of the matrix in a special "vector" created by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- x$getinv()
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }

  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  
  inverse

}
