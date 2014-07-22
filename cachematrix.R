## Put comments here that give an overall description of what your
## functions do
### These functions will be used to create a special object that stores
### a matrix and cache's its inverse to avoid future computings

## Write a short comment describing this function
### Creates a special 'matrix', which is really a list containing
### (pair values) functions to: set/get value of matrix, set/get value its inverse

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y){
          x <<- y
          inverse <<- NULL
     }
     get <- function() { x }
     setInverse <- function(solve) { inverse <<- solve }
     getInverse <- function() { inverse }
     # Returns list of functions to get/set  value of matrix/inverse
     list(set = set, get =get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
### First checks if inverse has alredy been calculated to return it from cache, (skips computing)
### If it hasn't been cached, then calculates the inverse of that 'special' matrix 
### returned before (really a list) and sets it in the cache via setInverse function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inverse <- x$getInverse()
     if(!is.null(inverse)){
          message("getting cached inverse matrix")
          return(inverse)
     }
     data <- x$get()
     inverse <- solve(data, ...)
     x$setInverse(inverse)
     inverse
}
