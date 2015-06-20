## This file contains the following functions:
## - makeCacheMatrix(): create a cache for the matrix inverse
## - cacheSolve(): return the cache of the matrix inverse  
## - runTests(): runs simple tests

## The makeCacheMatrix() wraps a matrix x inside 
## it's internal env and returns a list of
## functions:
##  - get: to get the matrix 
##  - set: to set a updated matrix
##  - getinv: to get the cached inv of the matrix
##  - setinv: to set the cached inv of the matrix
## This funciton acts as a cache for the inverse of x,
## if the matrix is updated this function will reset the 
## cached inverse if the new matrix is different
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

## The cacheSolve() takes the list returned by the makeCacheMatrix()
## and returns the inverse of the matrix wrapped by the makeCacheMatrix()
## It first checks if there is a cached inverse if not it computes the 
## inversed matrix and stores it as a cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

runTest <- function() {
  m <- matrix(1:4, 2,2)
  mc <- makeCacheMatrix(m)
  print('ok--- we made a cached matrix:')
  print(mc$get())
  
  inversed <- cacheSolve(mc)
  
  print('            get an inversed matrix:')
  print(inversed)
  
  print('redo            an inversed matrix:')
  print(cacheSolve(mc))
  
  print('redo one more time inversed matrix:')
  print(cacheSolve(mc))
  
  print('change the initial matrix:')
  m[1,1] <- 10
  mc$set(m)
  
  print(mc$get())
  
  print('           the new inversed matrix:')
  print(cacheSolve(mc))
  
  print('redo       the new inversed matrix:')
  print(cacheSolve(mc))
}
