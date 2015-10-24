## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
##set the value of the Matrix
##get the value of the Matrix
##set the value of the inverse
##get the value of the inverse
## the cache variable inv must be initialized to NULL

makeCacheMatrix <- function(x = matrix()) {
  inv  <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
##This function computes the inverse of the special "vector" 
## First it checks to see if the inverse has been calculated and cached with the if(!is.na)
## if cached data exists it skips the calculation and return the cached inverse.
## if no cached data exists then it computes the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    ## This returns the cached matrix if one exists
    return(inv)
  }
  data <- x$getMatrix()
  ##Computing the inverse of a square matrix can be done with the solve function in R. 
  ## Straight from the assigment prompt
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
