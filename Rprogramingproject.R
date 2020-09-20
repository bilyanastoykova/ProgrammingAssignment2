###The function "makeCacheMatrix" creates a special "matrix" object 
###that can cache its inverse.
###The function "cacheSolve" computes the inverse of the special 
###"matrix" returned by "makeCacheMatrix". 

## Here "x" is a matrix and inv is null 

makeCacheMatrix <- function(x = matrix(1:50,25,2)) {
  inv <- NULL
set <- function (y) {
  x <<- y
  inv <<- NULL
}
get <- function () x
setInverse <- function (inverse) inv <<- inverse
getInverse <- function () inv 
list(set = set,
     get = get,
     setInverse = setInverse,
     getIverse = getInverse)
}


## Here the inversed special matrix 'x' is returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(s)) {
    message("getting matrix that is inversed")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
