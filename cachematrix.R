## Put comments here that give an overall description of what your
## functions do

## Takes a matrix object as an input and returns a list of 4 functions which do the following:
## set: assigns a new matrix to the matrix variable x, and assigns NULL to the inverted matrix variable
## get: returns the matrix (not inverted)
## setinverse: assigns a new matrix to the inverted matrix variable i.
## getinverse: returns the inverted matrix variable i.

makeCacheMatrix <- function(x = matrix()) {
  i = NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  return(list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse))
}


## This function takes the output of makeCacheMatrix as its input
## and returns an associated inverted matrix. The first time it is run,
## it will cache the inverted matrix using the setinverse function created in makeCacheMatrix
## so that the caches version can be returned upon subsequent calls instead of running solve on each call.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  return(i)
}
