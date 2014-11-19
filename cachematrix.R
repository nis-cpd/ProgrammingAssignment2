## Compute the inverse of a matrix, cacheing the result for future reuse.  If
## the inverse exists in the cache then the cached value is returned, thereby
## improving performance.

## Creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
	        x <<- y
	        m <<- NULL
	}
	# Get the underlying matrix
	get <- function() x
	# Cache its inverse
	setinverse <- function(inverse) m <<- inverse
	# Retrieve its inverse from cache
	getinverse <- function() m
	
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## Computes the inverse of the special matrix object returned by
## mackCacheMatrix.  If the inverse has been calculated previously and the
## matrix has not changed, returns the cached value instead of computing it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    # Use cached value if available
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    # Did not exist in cache, therefore compute using 'solve'
    data <- x$get()
    m <- solve(data, ...)
    # then cache result for later
    x$setinverse(m)
    m
}
