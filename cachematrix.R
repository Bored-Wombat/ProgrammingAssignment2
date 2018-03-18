## These functions make a matrix that can cache it's inverse.
## 

## This function returns a list of the functions set, get, setinv and getinv

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(presolved) minv <<- presolved
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns the data cached in $getinv unless it is NULL, in which case it solves the matrix
## sets the $setinv of the CacheMatrix to the inverse just calculated, and returns this value.

cacheSolve <- function(x, ...) {
  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
        ## Return a matrix that is the inverse of 'x'
}