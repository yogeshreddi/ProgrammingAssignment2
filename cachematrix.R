## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The function makeCacheMatrix returns a list of functions to
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse of matrix
##4.get the value of the inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function

##The function will compute the inverse of a matrix created with makeCacheMatrix().
##If the inverse already calculated before it will return the cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
