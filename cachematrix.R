## Functions for Programming assignment 2 - R Programming
## makeCacheMatrix and cacheSolve are used to calculate and store/cache
## the inverse of a matrix.
## programmer: HHStarling 20150914

## TO USE THE FUNCTIONS:
## 1.source this file in working directory
## 2.create matrix and assign to list of functions in makeCacheMatrix
## my_matrix <- makeCacheMatrix (matrix(1:4, nrow=2, ncol=2))
## 3.then solve for inverse matrix using cacheSolve
## cacheSolve(my_matrix)

## *****Functions below *****

## makeCacheMatrix creates a special "matrix" object that can cache its inverse matrix
## last updated: 20150914
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## set function to set the value
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  ## get function to retrieve the value
  get <- function() x
  
  ## set the value of the inverse
  setinverse <- function(solve) m <<- solve
  
  ## get the value of the inverse
  getinverse <- function() m
  
  ## store these internal functions in a list for the main function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## the makeCacheMatrix function above. If the inverse has already been 
## calculated, then function returns inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## if value exists already then return the cached value
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## value does not already exist, so calculate inverse, return value 
  ## and save to cache for next time
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
