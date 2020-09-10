## cachematrix.R
##
## Creates a cached marix that can be used to repeatably solve the inverse of the marix.
## Makes it clear when a matrx has a cached value already calculated or if it needs to be solved.
##
## Usage:
##  mat <- matrix(c(6, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(mat)
##  cacheSolve(cacheMatrix)
##
##  cacheMatrix$set(mat)      # Store the matrix being cached.
##  mat <- cacheMatrix$get()  # Return the matrix being cached.
##

## Creates a cacheMatrix object for an invertable matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Sets the inverse of a cacheMatrix object.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
