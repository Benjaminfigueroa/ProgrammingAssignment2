makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversa <- function(inversa) m <<- inversa
  getinversa <- function() m
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}

cacheSolve <- function(x, ...) {
  m <- x$getinversa()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversa(m)
  m
}