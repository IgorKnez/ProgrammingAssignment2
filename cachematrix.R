## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
## this list is used as the input to cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
    
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  
  list(set = set, get =get, setinverse = setinverse, getinverse =getinverse)
  
}


## Write a short comment describing this function
## x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  # if the inverse has already been calculated:
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse:
  
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  return(inv)
  
}
