# Computing the inverse of a matrix is an expensive operation. The
# functions below will allow this computation to be cached, allowing
# repeated computations to complete quickly.

# Creates an object used to cache and compute the inverse of a matrix.
makeCacheMatrix <- function(obj.matrix = matrix()) {
  obj.inverse <- NULL
  set <- function(m) {
    obj.matrix <<- m
    # clear the cache because a new matrix is being used.
    obj.inverse <<- NULL
  }
  get <- function() obj.matrix
  setinverse <- function(i) obj.inverse <<- i
  getinverse <- function() obj.inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Computes the inverse of a matrix, using cached results when possible.
cacheSolve <- function(mcm, ...) {
  inverse <- mcm$getinverse()

  # computes and caches the matrix inverse if it is null
  if (is.null(inverse)) {
    inverse <- solve(mcm$get())
    mcm$setinverse(inverse)
  }

  inverse
}
