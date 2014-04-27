## The following functions, makeCacheMatrix and cacheSolve, cache 
## the inverse of an invertible matrix.

## The makeCacheMatrix is designed to generate a matrix object  
## that will be able to cache its inverse.  It will produce a 
## special matrix that will set the matrix, get the matrix,
## set the inverse matrix, and get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y = matrix()) ## set the matrix 
    {
    x <<- y
    m <<- NULL
    }
    get <- function() x           ## get the matrix
    setinverse <- function(inverse=matrix()) m <<- inverse
                                  ## set the inverse matrix
    getinverse <- function() m    ## get the inverse matrix
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
  }
  
  
## The cacheSolve function will calculate the inverse of the matrix 
## from makeCacheMatrix.  If the inverse has already been computed,
## the function will get the inverse from the cache.  If not, it will
## solve, and then return, the inverse matrix.

cacheSolve <- function(x = matrix(), ...) {
                                  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {               ## if inverse exists
    message("getting cached data")
    return(m)                     ## get it
  }
  data <- x$get()                 ## if no inverse has been calculated
  m <- solve(data, ...)           ## solve for the inverse
  x$setinverse(m)                 ## sets the inverse
  m                               ## display the inverse
}