## This program is to compute the inverse of a matrix and cache it so that it can be retrieved when required.
## If not available in cache it computes the inverse of matrix and returns it.

## Course: Introduction to R
## Assignment 2

## This function creates a matrix object and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
