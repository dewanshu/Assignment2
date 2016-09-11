## This program takes a matrix and returns its inverse either by 
## calcuting it or by returning previously calculated values stored
## by caching.

## This function takes a matrix as input, performs caching and 
## returns a list of functions.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Takes the list returned by upper function and returns inverse
## of the given matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inverse <- solve(data)
  c <- x$setinverse(inverse)
  return(c)
}
