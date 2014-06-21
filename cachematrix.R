## makeCacheMatrix function returns a list with functions to get/set 
## matrix/inverse matrix

makeCacheMatrix <- function(a = matrix()) {
  sm <- NULL
  set <- function(b) {
    a <<- b
    sm <<- NULL
  }
  get <- function() a
  setsolve <- function(solve) sm <<- solve
  getsolve <- function() sm
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}

## Calculates inverse matrix, but firstly check if it was already calculated, 
## if was , function returns value without any calculation.

cachesolve <- function(a) {
  sm <- a$getsolve()
  if(!is.null(sm)) {
    message("getting cached data")
    return(sm)
  }
  data <- a$get()
  sm <- solve(data)
  a$setsolve(sm)
  sm
}