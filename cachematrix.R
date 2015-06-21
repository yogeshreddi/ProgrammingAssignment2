## makeCacheMatrix() and cacheSolve() functions are used to compute and cache the inverse of a matrix.
## functions will only work for nonsingular or invertible matrix

##about makeCacheMatrix()
##The function makeCacheMatrix returns a list of functions to
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse of matrix
##4.get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
##setting the input matrix(storing) 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x                             ## getting the stored input matrix
  setinverse <- function(inverse) inv <<- inverse ## setting the inverse of the input matrix
  getinverse <- function() inv
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

##about cacheSolve()
##The function will compute the inverse of a matrix created with makeCacheMatrix().
##If the inverse already calculated before it will return the cached inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
##Checking if we have cached the inversed of the matrix 
    if(!is.null(inv)) {
    message("getting cached data")
    return(inv)                        ##Retuning already cached inverse
    }
## Computing the inverse of the matrix if the inverse isn't calculated and cached yet
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)                          ## Return a matrix that is the inverse of 'x'
}

#####Examples

## First example

##> x<-matrix(c(1,2,6,8),2,2)
##> m<-makeCacheMatrix(x)
##> cacheSolve(m)
##     [,1]  [,2]
##[1,] -2.0  1.50
##[2,]  0.5 -0.25
##> m$get()
##     [,1] [,2]
##[1,]    1    6
##[2,]    2    8
##> m$getinverse()
##     [,1]  [,2]
##[1,] -2.0  1.50
##[2,]  0.5 -0.25

##second example
## taking a new vector y and trying to cache its inverse

##> y<-matrix(c(3,7,8,11),2,2)
##> m<-makeCacheMatrix(y) 
##> m$getinverse()

##  NULL   ----because we didn't caluculate and set the inverse of new matrix y--

##> cacheSolve(m)
##        [,1]       [,2]
##[1,] -0.4782609  0.3478261
##[2,]  0.3043478 -0.1304348
##> m$getinverse()
##        [,1]       [,2]
##[1,] -0.4782609  0.3478261
##[2,]  0.3043478 -0.1304348
###Please comment on the work, Thank you#####