
  ## makeCacheMatrix: This function makes a special "matrix" object
  ## that can cache its inverse.
  ## cacheSolve computes the inverse of the special "matrix" returned
  ## by makeCacheMatrix .If the inverse has already been calculated 
  ## and the matrix has not changed. 
  ## Then the cachesolve  retrieves the inverse from the cache.


    ## The  function, makeCacheMatrix makes a special "vector",
    ## which is  a list containing a function to
    
    ## set the value of the matrix
    ## get the value of the matrix
    ## set the value of the inverse
    ## get the value of the inverse
    
    
    
    
    makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
    }
    
    
    ## The following function calculates the inverse of the special "matrix" 
    ## made with the above makecacheMatrix function.However, it first checks to see 
    ## if the inverse  has already been calculated. If so, it gets the inverse 
    ## from the  cache and skips the computation. Otherwise, it calculates the 
    ## inverse of the data and sets the value of the inversion in the cache via the
    # #setinverse function.
    
    cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
    }