## Put comments here that give an overall description of what your
## functions do
## The functions below can be used to create a "matrix" object that can cache its inverse,
## for this assignment, we assume that the matrix supplied is always invertible, 
## The functions are similar to the ones explained in the example, though the "mean" 
## function is substitute by inverse function

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" object created by 
## makeCacheMatrix function above. It first check to see if the inverse has 
## already been calculated, then it should return the inverse from the cache and 
## skip the computation. Otherwise, it calculates the inverse of the matrix and sets 
## the value in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
  message("getting cached data")
  return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
        ## Return a matrix that is the inverse of 'x'
}
