## Calculates and caches the inverse of a matrix

## Creating a special "matrix" that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
## Returning the matrix that is the inverse of x, as cached by makeCacheMatrix above
## If inverse is already calculated, cacheSolve retrieves that inverse. Otherwise, it calculates using solve()
cacheSolve <- function(x, ...) {
  + inv <- x$getinv()
  + if(!is.null(inv)) {
    + message("getting cached data")
    + return(inv)
    + }
  + data <- x$get()
    + inv <- solve(data, ...)
  + x$setinv(inv)
  + inv
}
