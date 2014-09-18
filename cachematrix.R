### makeCacheMatrix Function - Creates the CacheMatrix object which
### Holds the passed Matrix and, if solved, a cached copy of the 
### Matrix's inverse.
###
### Input Parameter - A Matrix (preferrably invertable/square)
### Methods:
###     set - Setter
###     get - Getter
###     setinverse - Sets the Inverse cache
###     getinverse - Gets the cached Inverse (possibly NULL)
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL    # Sets the initial state of the Inverse cache
    set <- function(y) {
        x <<- y        # Set the Matrix to whar was passed
        i <<- NULL     # Invalidate the cache
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


### cacheSolve Function - Returns the Inverse of the passed CacheMatrix
### by either returning the cached value or performing the solve() (and
### caching the result.
###
### Input Parameter - A CacheMatrix Object *created via the 
###                   "makeCacheMatrix" function
### Returns:
###     Success - Inverted Matrix (either from cache or "solve()")
###     Graceful Fail - Matching Matrix of "NA" if Matrix not
###                     invertable/square.
cacheSolve <- function(x, ...) {
    invMatrix <- x$getinverse()     # get te current inverse cache value
    if(!is.null(invMatrix)) {
        # Current value is NOT NULL, so use cached value.
        message("getting cached data")
        return(invMatrix)
    }
    
    data <- x$get()     # Get the CacheMatrix Matrix data
    
    # Check if the Matrix is invertable/square.
    if (nrow(data) != ncol(data)) {
        # Matrix not invertable (not square)
        message("Matrix not invertable (not square), returning NA's")
        return(matrix(nrow = nrow(data), ncol = ncol(data)))
    }

    # Solve and store the Inverted Matrix, then return it.
    invMatrix <- solve(data, ...)
    x$setinverse(invMatrix)
    return(invMatrix)
}
