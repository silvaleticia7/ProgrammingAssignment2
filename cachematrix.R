## The assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a matrix. 
##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  set = function (y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x 
  setinv = function(inverse)  inv <<- inverse
  getinv = function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The following function returns the inverse of the matrix created with the above function. 
##However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the result from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)){
    message("getting cached data")
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}   
