## This pair of functions establishes, and provides access to, an augmented 
## matrix object that has a cache for its inverse.

## This function creates the augmented "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  # i represents the cached inverse value
  i <- NULL
  
  # set updates the object with a new matrix, clearing the cached inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get returns the underlying matrix object
  get <- function() x
  
  # setinverse caches the supplied inverse value
  setinverse <- function(inverse) i <<- inverse
  
  # getinverse returns the cached inverse value
  getinverse <- function() i
  
  # return a list with the functions that close over the 
  # matrix x and its inverse i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function returns the inverse of the augmented "matrix" returned 
## by makeCacheMatrix above. If the inverse has not already been calculated 
## cacheSolve calculates it and caches it.  Finally cacheSolve returns the
## cached value.
## 
## The invariant that the cached value (when present) always corresponds
## to the matrix value is maintained by the set function of the augmented
## matrix object which clears the cache when setting a new matrix value
## thereby signalling the need to calculate the inverse anew.
##
## Example:
##
## > z <- rnorm(1000 * 1000)
## > m <- matrix(z, 1000, 1000)
## > cm <- makeCacheMatrix(m)
##
## > system.time(cacheSolve(cm))
##   user  system elapsed 
##   1.192   0.002   1.194 
##
## > system.time(cacheSolve(cm))
## getting cached data
##   user  system elapsed 
##   0.001   0.000   0.001 

cacheSolve <- function(x, ...) {
  
  # check the cache for an existing inverse 
  i <- x$getinverse()
  
  if (is.null(i)) {
    # cache miss, caculate 
    data <- x$get()
    i <- solve(data, ...)
    
    # store invese in cache
    x$setinverse(i)
  } else {
    # cache hit, share the good news
    message("getting cached data")
  }
  
  # at this point the inverse was either in the cache initially
  # or we just calculated it and put it there. Regardless, i is 
  # the inverse now and we can return it.
  i
}
