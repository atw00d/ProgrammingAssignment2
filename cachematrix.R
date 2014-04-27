## to cache the matrix inversion and save up the time for recalculating it

## creating new class of vector for matrix which can store the caluclated matrix inversion (if any) 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y=matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## to test whether cache matrix inversion exist, if not, calculate it and store the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
