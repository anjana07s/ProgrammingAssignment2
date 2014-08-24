## The function gets a matrix as input,calculates the inverse and caches it 

## The function gets a matrix as input,calculates the inverse and returns
## a list of functions that saves the inverse matrix and returns it.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function gets inverse matrix as input and if its not a null value
##(meaning matrix is unchanged), then retrieves the value from chache, 
## else calculates the new inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i = x$getinverse()
  if(!is.null(i)) {
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}

x <- matrix(1:4,2,2)
cacheSolve(makeCacheMatrix(x))
