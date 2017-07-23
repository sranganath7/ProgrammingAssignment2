## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  
  # holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
inv <- NULL

# store a matrix
set <- function(y){
  x <<- y
  # since the matrix is assigned a new value, flush the cache
  inv <<- NULL
}
# returns the stored matrix
get <- function() {
  x
}

# cache the given argument 
setinv <- function(inverse) {
  inv <<- inverse
}

# get the cached value
getinv <- function() {
  inv
}

# return a list. Each named element of the list is a function
list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  # if a cached value exists return it
  
  if(!is.null(inv)){
    message("getting inverse data")
    return(inv)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  
  # return the inverse
  inv
}

