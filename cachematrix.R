## This function helps in storing and retrieving the inverse. It does not compute any inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse if it has not been already been calculated. If inverse is already available, it retrieves it from the cached data.

cacheSolve <- function(x, ...) { 
  inv <- x$getinverse()
  if(!([inv count] == 0) ) {
    message("getting cached data")   ## If inverse has been calculated previously, reading it from cache.
    return(inv)
  }
  matrix <- x$getinverse()  # Matrix is received from parent.
  inv <- solve(matrix)  # Matrix is solved for inverse
  x$setinverse(inv)  ## Inverse calculated is stored to parent environment.
  inv    ## Inverse is returned
}
