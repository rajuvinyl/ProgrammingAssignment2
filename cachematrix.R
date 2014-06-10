## The below functions perform caching of the Inverse of a Matrix

## It creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
		## setting the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		## getting the value of the matrix
        get <- function() x
		## setting the value of the inverse
        setinverse <- function(inverse) m <<- inverse
		## getting the value of the inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## It calculates the inverse of the special "matrix" created with the above function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
		## Checking if a cached copy of the inverse exists and using it if it does
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
		## Computing the inverse
        m <- solve(data, ...)
		## Caching the computed inverse
        x$setinverse(m)
        m
}
