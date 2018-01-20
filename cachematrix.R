


# makeCacheMatrix creates a special matrix which is a list that contains a
# function to:
# set the value of the matrix, get the value of the matrix,
# set the value of the inverse of the matrix,
# retrieve the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y){
        x <<- y
        x_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x_inv <<- inverse
  getinverse <- function() x_inv
  list (set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve computes the inverse of the matrix created with makeCacheMatrix.
# If the inverse already exists and the matrix has not changed, it retrieves 
# the inverse from the cache

cacheSolve <- function(x, ...) {
  x_inv <- x$getinverse()
  if (!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data)
  x$setinverse(x_inv)
  x_inv
 
}
