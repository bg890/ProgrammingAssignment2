## Functions to find inverse of a matrix and store inverse in cachw

## makeCacheMatrix creates a matrix object that can cache the inverse of the special matrix.
makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL
  
  ## set the value of the special matrix
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  
  ## get the value of the special matrix
  get <- function() x
  
  #set the value of the inverse in caqche
  setsolve <- function(solve) s <<- solve
  
  ## get the value of the inverse of the special matrix
  getsolve <- function() s
  
  ##return a list of defined functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## cacheSolve computes the inverse from the makeCacheMatrix function. 
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of special matrix 'x'
  s <- x$getsolve()
  
  ## check if there is a inverse of the special matrix in cache
  if(!is.null(s)) { 
    
    ##If the inverse is already in cache, then cacheSolve will retrieve the inverse from cache
    message("getting cached data")
    return(s)
  }
  
  ## if inverse is not already in cache, then retrieve the special matrix from cache
  data <- x$get()
  
  ##Computing the inverse of the special matrix
  s <- solve(data, ...)
  
  ## set the inverse of the special matrix in cache
  x$setsolve(s)
  s
}
