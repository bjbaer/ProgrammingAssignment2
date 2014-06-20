## This is a pair of functions that will invert a matrix and store the result. The next time the function is called
## if it is called to invert the same matrix it will output the stored result.

##This is the function that makes the blank matrix and stores it, this is not the function that actually inverts
##a matrix, but checks to see if it is the stored matrix
makeCacheMatrix <- function(x = matrix()) {
  ##setting the variable to null to check if this matrix has already been inverted.
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## this is what checks to see if the matrix has already been inverted and if so returns my message
  if(!is.null(m)) {
    #Return the message if the inverse of the matrix is already calculated
    message("I already have this written down, let me find it....")
    #then return the inverted matrix
    return(m)
  }
  #invert the matrix if it hasn't already been inverted
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
