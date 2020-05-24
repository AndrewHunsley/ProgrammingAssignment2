## this function sets the has a couple steps
## firstly setting the inverse of the matrix and 
## calling the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  INV <- NULL
  set <- function(y) {
    x <<- y
    INV <<- NULL
  }
  get <- function()x
  SETINVERSE <- function(INVERSE) INV <<- INVERSE
  GETINVERSE <- function() INV
  list(set = set, get = get, SETINVERSE = SETINVERSE, GETINVERSE = GETINVERSE)
}

##this section has a few steps
##call the function above, if not null
##inverse matrix
##solve data 
##return inverse output

cacheSolve <- function(x, ...) {
  INV <- x$GETINVERSE()
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  }
  data <- x$get()
  INV  <- solve(data,...)
  x$SETINVERSE(INV)
  INV
}
