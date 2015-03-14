## Function caching the matrix:
# 1. Set the value of the matrix
makeCacheMatrix <- function(mat = matrix()) {
  mat_inv <- NULL
  set <- function(y) {
    mat <<- y
    mat_inv <<- NULL
  }
  # 2. Get the value of the matrix
  get <- function() mat
  # 3. Set the value of inverse of the matrix
  setinverse <- function(inverse) mat_inv <<- inverse
  # 4. Get the value of inverse of the matrix
  getinverse <- function() mat_inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
# For this assignment, we assume that the matrix supplied is always invertible.
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  cacheSolve <- function(mat, ...) {
  mat_inv <- mat$getinverse()
  if(!is.null(mat_inv)) {
    # If the inverse has already been calculated (and the matrix has not changed), 
    # then the cachesolve should retrieve the inverse from the cache.
    return(mat_inv)
  }
  data <- mat$get()
  mat_inv <- solve(data)
  mat$setinverse(mat_inv)
  # Printing the inverted matrix
  mat_inv
}