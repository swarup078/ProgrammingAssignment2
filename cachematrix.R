makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinver <- function() i <<- solve(x)
  getinver <- function() i
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  i <- x$getinver()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinver(i)
  i
  ## Return a matrix that is the inverse of 'x'
}

