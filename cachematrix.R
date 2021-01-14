## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## x is an invertible matrix
  ## the function sets and gets the matrix
  ## it sets and gets the inverse matrix of x
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inv <- function(inverse) m <<- inverse
  get_inv <- function() m
  list(set = set,
       get = get,
       setinverse = set_inv,
       getinverse = get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## the function returns a matrix that is the inverse of 'x'
  ## checks if the inverse has already been calcuated
  ## if it hasn't, the function calculates and sets the inverted matrix
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m        
}
