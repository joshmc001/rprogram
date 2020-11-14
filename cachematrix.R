## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
   #establish matrix in cache environment
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  ## return inverse from cache environment if it is already set
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  ## else get the matrix from this function
  mat <- x$get()
  ## Calculate & assign the inverse matrix using matrix multiplication
  j <- solve(mat,...)
  ## Set the inverse matrix to the function
  x$setInverse(j)
  j  # Return the inverse matrix
}
