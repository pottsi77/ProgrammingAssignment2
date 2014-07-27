## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#below is a function that creates a matrix object that can cash it's inverse using SOLVE
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
#below is a function that calculates the inverse of a matrix, it will 
#check for a cached version of the calculation if it is available first
cacheSolve <- function(x, ...) {        
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- as.matrix(x$get())
  m <- solve(matrix, ...)
  x$setsolve(m)
  m  
}