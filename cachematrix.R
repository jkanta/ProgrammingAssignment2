## This set of utility scripts allows for the inversion of matrices
## with an attempt at caching optimization

## makeCacheMatrix will store the value of a matrix 
## set() stores the matrix
## get() returns the original matrix
## setinverse() will set the inverse of the matrix
## getinverse() will return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y  #store y in the static of x
    m <<- NULL
  }
  get <- function() x  #just returns x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}

## Check to see if the inverse for that matrix has already been created
## If not, compute the inverse and use the makeCacheMatrix$setinverse function to set it
cacheSolve <- function(x, ...)
{
  m<- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<- solve(data)
  x$setinverse(m)
  m
}
