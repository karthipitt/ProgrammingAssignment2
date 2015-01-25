## This function helps in storing and retrieving the inverse. It does not compute any inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {					## Setting a matrix
    x <<- y
    inv <<- NULL
  }						
  get <- function() x					## Retrieving a matrix	
  setinverse <- function(inverse) inv <<- inverse	## Setting inverse of matrix
  getinverse <- function() inv				## Retrieving inverse of matrix
  list(set = set, get = get,	
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse if it has not been already been calculated. If inverse is already available, it retrieves it from the cached data.

cacheSolve <- function(x, ...) { 
  inv <- x$getinverse()
  if(!([inv count] == 0) ) {
    message("getting cached data")   			## If inverse has been calculated previously, reading it from cache.
    return(inv)
  }
  matrix <- x$getinverse()  				## Matrix is received from parent.
  inv <- solve(matrix)  				## Matrix is solved for inverse
  x$setinverse(inv)  					## Inverse calculated is stored to parent environment.
  inv    						## Inverse is returned
}
