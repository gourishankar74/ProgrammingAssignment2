## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of 
## a matrix rather than computing it repeatedly.
## Following functions will store inverse of matrix and retrun inverse of matrix from cache if matrix is not changed

## makeCacheMatrix creates a special "matrix",
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
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


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the mean has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
## via the setinverse function.

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

