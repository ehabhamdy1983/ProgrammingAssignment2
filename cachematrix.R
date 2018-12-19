## This pair of functions can be used to cache the inverse of a matrix

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {        ## Takes in an input matrix from user
    x <<- y
    inv <<- NULL              ## Erases previously saved inverse
  }
  get <- function() x         ## Returns matrix
  setinv <- function(inver) inv <<- inver   ## Stores calculated inverse
  getinv <- function() inv                  ## Returns inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  ## Returns a list of four functions, to be called by used in runtime
}

## This function computes the inverse of the matrix returned by the above
## function. If the inverse has already been calculated, the cachesolve
## retrieves it from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()           ## Returns the cached inverse
  if(!is.null(inv)) {         ## If the stored value in cache is not NULL...
    message("getting cached data")
    return(inv)               ## Returns the inverse and exits function 'cacheSolve'
  }
  ## If no value was stored in cache, the next set of lines are executed
  data <- x$get()             ## Returns the matrix  
  inv <- solve(data, ...)     ## Calculates the inverse
  x$setinv(inv)               ## stores the inverse in cache
  inv                         ## Returns the inverse
}
