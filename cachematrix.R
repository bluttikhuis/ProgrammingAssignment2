## The first function, 'makeCacheMatrix', stores the input matrix as well as its cached inverse,
## which is or is not created by 'cacheSolve'. 'cacheSolve' is a function who checks if 'makeCacheMatrix' 
## did store the inverse matrix and, if it didn't, creates the inverse of the original matrix (which
## was put in as an argument for 'makeCacheMatrix').

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cacheimat <- NULL
  set <- function(y = matrix()) {
    x <<- y
    cacheimat <<- NULL
  }
  get <- function() x
  setimat <- function(imat) cacheimat <<- imat
  getimat <- function() cacheimat
  list(set = set, get = get, setimat = setimat, getimat = getimat)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  imat <- x$getimat()
  if(!is.null(imat)) {
    message("getting cached data")
    return(imat)
  }
  getx <- x$get()
  imat <- solve(getx, ...)
  x$setimat(imat)    
  imat
}
