## The two functions in this document are used to firstly create a matrix object which can be used to create a cached ivnerse vector
## The second function is what actually creates the cached inverse vector

## This function is to create a matrix object that is able to be used to create a cached inverse vector

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## This function allows the creation of a cached inverse vector or the return of a cached inverse if already existing

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse vector")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

