## The presented functions have the objective of creating a
## special object that stores a matrix and cache its inverse

## Creating a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setinv <- function(inv) mat <<- inv
  getinv <- function() mat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Inverting the matrix obtained previously

cacheSolve <- function(x, ...) {
  mat <- x$getinv()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)  ## Matrix inversion
  x$setinv(mat)
  mat  ## Return a matrix that is the inverse of 'x'
}

