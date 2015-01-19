## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Returns a list of getter and setter function defined in
# this functions closure. These getters and setters manipulate
# the cache variable which contains the matrix. The returned list
# is able to access the functions and therefore, the internally 
# managed cached variable to maniulate it (through the mini 
# environment) maintained in this makeCacheMatrix function.
makeCacheMatrix <- function(x = matrix()) {

  # Initialize the cache to NULL.
  cache <- NULL
  
  # Sets the new value of the matrix.
  set <- function(y = matrix()) {
    x <<- y
    cache <<- NULL # Reset the cache since the x value might have changed!
  }
  
  # Returns the current value of the matrix.
  get <- function() x
  
  # Sets the internal cache with the specified inverse value.
  setCache <- function(inv = matrix()) cache <<- inv
  
  # Returns the value of the internal cache or NULL if no such value exists.
  getCache <- function() cache
  
  # Create and return the list representing the 'cachable matrix' object.
  list(set = set,
       get = get,
       setCache = setCache,
       getCache = getCache)
}


## Write a short comment describing this function

# Returns the inverse of the specified 'cachable matrix' created
# by the makeCacheMatrix above. Before computing the value for 
# the inverse, this function however checks the state of the 
# internal cache maintained by the 'cachable matrix'. If it 
# finds it NULL (thus never used), is computes the inverse and
# sets the new value in the cache. If the value is already present
# in the internal cache of the 'cachable matrix' it immediately
# returns this value and exits.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Retrieve the cached value.
  cache <- x$getCache()
  
  # Check if the cache is already filled.
  if (!is.null(cache)) {
    message("Getting cached data")
    return(cache)
  }
  
  message("Computing inverse data")
  
  # If we're at this stage, then cache was empty - populate it.
  inv <- solve(x$get(), ...)
  
  # Store inverse value in cache.
  x$setCache(inv)
  
  # Finally return the newly computed inverse.
  inv
}

# Example test.

# Create a new 'cachable matrix'.
myMat <- makeCacheMatrix(matrix(rnorm(25), 5))

# Compute the inverse of the 'cachable matrix'. The inverse should be computed.
cacheSolve(myMat)

# Computer the inverse of the 'cachable matrix'. The inverse should not be computed
# but retrieved from the cache.
cacheSolve(myMat)

# Re-set the internal value of the 'cachable matrix'. At this stage, the
# cache should be invalidated and set to NULL.
myMat$set(matrix(rnorm(25), 5))

# Compute the inverse of the 'cachable matrix'. The inverse should be computed since
# the last cached value was invalidated and set to NULL.
cacheSolve(myMat)

# Computer the inverse of the 'cachable matrix'. The inverse should not be computed
# but retrieved from the cache.
cacheSolve(myMat)