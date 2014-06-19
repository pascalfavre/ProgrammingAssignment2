## Author:  Pascal Favre
## Date:    19.06.2014
## Comments:
##          Matrix inversion is usually a costly computation and
##          there may be some benefit to caching the inverse of a
##          matrix rather than computing it repeatedly.


##Name:     makeCacheMatrix
##Params:   x : matrix that will be used in this function
##Returns:  a matrix object which contains methods to get/set the
##          value of a matrix and get/set the cached inverse
##Comments: Given a matrix, this function can cache and return it's
##          inverse value
makeCacheMatrix <- function(x = matrix()) {
  #Set inv to NULL
  inv <- NULL
  
  #set(): function to set value to matrix to use in function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #get(): return value of matrix used in function
  get <- function() x
  
  #setinverse(): save the inverse matrix into cache
  setinverse <- function(inverse) inv <<- inverse
  
  #getinverse(): get the value of the inverse matrix
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##Name:     cacheSolve
##params:   x:    matrix returned by the makeCacheMatrix function
##          ...:  any other arguments useful for the solve() function
##returns:  returns the inverse of the x matrix.
##comments: Given a matrix object returned by the makeCacheMatrix, calculate the
##          inverse of the matrix or return it's cached value.
cacheSolve <- function(x, ...) {
  
  #Call for the inverse of the given matrix
  m <- x$getinverse()
  
  #if the result isn't null, get the cached matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if the inverse isn't cached, get the original matrix
  data <- x$get()
  #solve the original to get the inverse
  m <- solve(data, ...)
  #store the inverse of the matrix
  x$setinverse(m)
  #returns the inverse of a matrix
  m
}


