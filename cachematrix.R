
## The code introduces two functions, makeCacheMatrix and cacheSolve
## makeCacheMatrix creates a special "matrix" object that can cache 
## its inverse, while cacheSolve computes the inverse of the same
## matrix, but returns the cached matrix if the inverse has been
## calculated.


## Creates a special matrix object with 4 functions that allow
## caching an inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL ##initialise a variable, inv, as null
	set <- function(y) { ##sets a new matrix for inverting
   x <<- y 
   inv <<- NULL #whenever a new matrix is set, inv returns to null
   }
   get <- function() { x } ##gets the current matrix
   setinv <- function(inverse) { inv <<- inverse } ##sets the inverse
   getinv <- function() { inv } ##retrieves the inverse
   list(set = set, get = get, 	
      setinv = setinv,
      getinv = getinv) ##creates a list of the 4 functions
}


## Finds the inverse for a matrix formed in makeCacheMatrix, but uses
## the cached value if it exists

cacheSolve <- function(x, ...) {
	inv <- x$getinv() ##gets the value of inv from the cached matrix
      if(!is.null(inv)) { ##if inv is not NULL, it has been cached
         message("getting cached data")
         return(inv) ##returns the cached inverse
      }
      data <- x$get() ##gets the current matrix to be cached
      inv <- solve(data, ...) ##solves for the inverse of the matrix
      x$setinv(inv) ##sets the value of inv
      inv  ## Return a matrix that is the inverse of 'x'
}
