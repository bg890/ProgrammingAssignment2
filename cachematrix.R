## Functions to find inverse of a matrix and store inverse in cachw

## makeCacheMatrix creates a matrix object that can cache the inverse of the special matrix.
makeCacheMatrix <- function(specialmatrix = matrix()) {
  
  inverse <- NULL
  
  ## set the value of the special matrix
  set <- function(y) {
    specialmatrix <<- y
    inverse <<- NULL
  }
  
  
  ## get the value of the special matrix
  get <- function() specialmatrix
  
  #set the value of the inverse in caqche
  setsolve <- function(solve) 
    inverse <<- solve
  
  ## get the value of the inverse of the special matrix
  getsolve <- function() inverse
  
  ##return a list of defined functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## cacheSolve computes the inverse from the makeCacheMatrix function. 
cacheSolve <- function(specialmatrix, ...) {
## Return a matrix that is the inverse of the special matrix
  inverse <- specialmatrix$getsolve()
  
  ## check if there is a inverse of the special matrix in cache
  if(!is.null(inverse)) { 
    
    ##If the inverse is already in cache, then cacheSolve will retrieve the inverse from cache
    message("getting cached data")
    return(inverse)
  }
  
  ## if inverse is not already in cache, then retrieve the special matrix from cache
  data <- specialmatrix$get()
  
  ##Computing the inverse of the special matrix
  inverse <- solve(data, ...)
  
  ## set the inverse of the special matrix in cache
  specialmatrix$setsolve(inverse)
  inverse
}
