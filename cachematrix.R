## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set the inverse of the matrix
  setinv <- function(inv_) inv <<- inv_
  getinv <- function() inv
  ## get the inverse of the matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
  inv <- x$getinv()
  ## Return a matrix that is the inverse of 'x'
  ## check if there is the matrix 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  ## if not: get the inverse of the matrix 
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
