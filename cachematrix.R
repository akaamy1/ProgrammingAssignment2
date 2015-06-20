## This pair of functions allow the user to cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {#define arguments of the function
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }#set the value of the matrix
  get <- function() x # get the value of the matrix
  setinverse <- function(solve) m <<- solve #set the value of the inverse
  getinverse <- function() m #get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...){#check to see if inverse of matrix has been calculated
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data") 
    return(m)
  }#if so, retrieves cached value
  data <- x$get #if not calculate inverse of matrix
  m <- solve(data, ...)
  x$setinverse(m)
  m ## Return a matrix that is the inverse of 'x'
}