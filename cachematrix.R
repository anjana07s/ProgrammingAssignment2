## The function gets a matrix as input,calculates the inverse and caches it. If the values of matrix is unchanged, 
## retrieves the values from cache, else calculates the inverse of this new matrix which is the input. 

## The function gets a matrix as input,calculates the inverse and caches it. 


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


## This function gets a  matrix as input and if its not a null value
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