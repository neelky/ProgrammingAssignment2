## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function will create a special object (matrix) that has the 
##capabilitiy to cache the inverse of itself thus reducing lengthy computations 

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set,get=get, setinverse=setinverse, getinverse <- getinverse)
}


## Write a short comment describing this function
## This function first calculates the inverse of special matrix object from the 
## previous function "MakeCacheMatrix". The caching will occur if the value has not
## changed and it can simply pull the cached value from the environment

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

