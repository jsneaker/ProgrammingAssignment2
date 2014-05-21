##    cachematrix.R includes two functions that cache the inverse of a matrix:
##    (1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##    (2) cacheSolve: This function computes the inverse of the special "matrix" returned by 
##    makeCacheMatrix above. If the inverse has already been calculated 
##    (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      
      ##create a null inverseM
      inverseM <- NULL
      
      ##This sets the matrix value
      set <- function(y= matrix()) {
            x <<- y
            inverseM <<- NULL
      }

      ##This returns the matrix
      get <- function() x
      
      ##This sets the inverseM value based on a provided value
      setInverse <- function(suppliedInverse) inverseM <<- suppliedInverse
      
      ##This will return the inverseM value
      getInverse <- function() inverseM
      
      ##This creates the list of functions
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


##    This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##    If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##    will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      
      ##This attempts to get the cached inverse
      myInverse <- x$getInverse()
      
      ##This tests to see if inverse value was cached and returns it if it is available
      if(!is.null(myInverse)) {
            message("getting cached data")
            return(myInverse)
      }
      
      ##get the inverse so that we can get calculate the inverse
      data <- x$get()
      
      ##calculate the inverse
      myInverse <- solve(data)
      
      ##cache the inverse
      x$setInverse(myInverse)
      
      ##return the inverse
      myInverse
}
