## In this program, we are using "makeCacheMatrix" and "cacheSolve" functions to
## create a special MATRIX object that can catche inverse and to return the inverse of matrix 
## returned by makeCacheMatrix

## makeCacheMatrix creates a special "matrix", where we set the value of the matrix, get the value of the matrix,
## set the value of the inverse or "solve" ,and get the value of the "solve"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Here if the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache. otherwise it will calculate new one
## and cache the inverse

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m   
   ## Return a matrix 'm' that is the inverse of 'x'
}
