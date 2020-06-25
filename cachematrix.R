## Put comments here that give an overall description of what your
## functions do
## Week 3 Assignment Title : Caching the Inverse of a Matrix

## Write a short comment describing this function
## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  ori <- NULL						## ori as NULL is initial matrix value
  set <- function(new){					## new is target new matrix
  	x <<- new						## buffer matrix
  	ori <<- NULL					## reset ori back to NULL 
  }
  get <- function()x					## return to matrix argument
  setInverse <- function(inverse) ori <<- inverse
  getInverse <- function() ori 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}

## Write a short comment describing this function
## This function computing the inverse of a square matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	ori <- x$getInverse()
  	if(!is.null(ori)){
  		return(ori)
  	}
  	old <- x$get()
  	ori <- solve(old,...)
  	x$setInverse(ori)
  	ori
}
