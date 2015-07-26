## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## input :a square invertible matrix
## Output: the list that is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  
  
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## input: output of makeCacheMatrix()
## output:A matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
 inv = x$getinv()
  
  # Cache check
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...) 
   x$setinv(inv)
        
        return(inv)
}