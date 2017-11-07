## Caching the Inverse of a Matrix
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL
  set <- function(y){
    x <<- y
    inverseM <<- NULL
  }
  get <- function() x
  setInverseM <- function(solve) inverseM <<- solve
  getinverse <- function() inverseM
  list(set = set, get = get,
       setInverseM = setInverseM,
       getinverse =getinverse)
}
## cacheSolve: This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
##has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseM <- x$getinverse()
  if(!is.null(inverseM)){
    message("getting cached data")
    return(inverseM)
  }
  matrixO <- x$get()
  inverseM <- solve(matrixO, ...)
  x$setInverseM(inverseM)
  inverseM
}
