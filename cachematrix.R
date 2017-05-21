## cachematrix.R file contains 2 functions named "makeCacheMatrix"
## and "cacheSolve". These functions used to compute and cache the 
## inverse of a matrix.

## makeCacheMatrix function will create a special matrix object
## that can cache the matix and its inverse. This function returns
## the list of getter and setter functions for the matrix and its inverse.


makeCacheMatrix <- function(x = matrix()) {
  mat_inverse <- NULL
  set <- function(y) {
    x <<- y
    mat_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mat_inverse <<- inverse
  getinverse <- function() mat_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## "CacheSolve" function computes the inverse of the special matrix object 
## which is returned by "makeCacheMatrix" function. It computes only when 
## the inverse if not computed yet. If the inverse is already cached then 
## it returns the cached inverse matrix. So in this way, it is much faster
## than the normal solve funtion.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat_inverse <- x$getinverse()
  if(!is.null(mat_inverse)) {
    message("getting cached data")
    return(mat_inverse)
  }
  data <- x$get()
  mat_inverse <- solve(data, ...)
  x$setinverse(mat_inverse)
  mat_inverse
}
