## This is the solution to Programming Assignment #2 in the Coursera 
## "R Programming" course
## 
## This file provides two functions:
## - makeCacheMatrix: this function is used to build a sort of matrix "object" 
##                    that has the ability to cache its inverse 
## - cacheSolve: this function is used to compute the inverse of the special 
##                    matrix "object" built by the makeCacheMatrix function
##
## This file also provides some example and test functions

## function is used to build a sort of "matrix object" that has the ability 
## to cache its inverse.
## This object expose some setter/getter functions used to set or retrieve 
## the actual matrix or its (cached) inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # When the "matrix object" is first build we null the chached inverse value
  cachedInverse <- NULL
  
  # This is the setter function of the matrix in the "matrix object"
  setMatrix <- function(y) {
    # Sets the value of the matrix in the parent environment
    x <<- y
    
    # nulls the value of the cached inverse in the parent environment
    cachedInverse <<- NULL
  }
  
  # This is the getter function of the matrix in the "matrix object"
  getMatrix <- function() x
  
  # This is the setter function of the inverse matrix in the "matrix object"
  setInverseMatrix <- function(inverse) cachedInverse <<- inverse
  
  # This is the getter function of the inverse matrix in the "matrix object"
  getInverseMatrix <- function() cachedInverse

  # the following statement returns a list (yes the "matrix object" is indeed
  # just a list) that is used to expose the getter/setter functions
  # defined above for the "matrix object"
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix, 
       setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix)
  
}

## This function computes the inverse of a matrix.
## It accepts in input a special "matrix object" (typically created using the 
## makeCacheMatrix function).
## The inverse is computed only the first time this function is invoked on
## a "matrix object", subsequent invocations return the inverse of the 
## matrix cahed in the "matrix object" itself.

cacheSolve <- function(x, ...) {
  
  # try if the pre-computed inverse is available from the cache
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)){
    message("Data retrieved from chache")
    return(inverseMatrix)
  }
  
  # retrieve the actual matrix from the "matrix object"
  theMatrix <- x$getMatrix()
  
  # do the computation
  inverseMatrix <- solve(theMatrix, ...)
  
  # save the camputed inverse matrix in the cache of the "matrix object"
  x$setInverseMatrix(inverseMatrix)
  
  # return the computed inverse matrix
  inverseMatrix
}
