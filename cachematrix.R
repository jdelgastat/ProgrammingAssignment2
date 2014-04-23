## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix takes an argument x of type matrix
## makeCacheMatrix returns a list with 4 list items:
##    1 set the value of the matrix
##    2 get the value of the matrix
##    3 set the value of the inverse
##    4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {       
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<-solve
  getsolve <- function() m
  list(set = set, get = get,                     
       setsolve = setsolve, 
       getsolve = getsolve)
}

## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated, the cachesolve should retrieve the inverse from the cache,
## and the message "getting cached data" is triggered.
## The input is expecting a special matrix made from makeCacheMatrix.
## The output is the inverse coming whether from the 
## special matrix cache or computation
  
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
} 
