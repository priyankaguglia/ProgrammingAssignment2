## couple of function which cache the inverse of a matrix using lexical scoping.

## First function creates a "matrix" object that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() 
    x
  setInverse <- function(solve)
    inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## Second function computes the inverse of the "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

 inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  
   ## Return a matrix that is the inverse of 'x'
   inv
   
}
