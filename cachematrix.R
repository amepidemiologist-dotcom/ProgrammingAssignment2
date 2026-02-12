## These functions work together to cache the inverse of a matrix.
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## cacheSolve retrieves the inverse from the cache instead of recomputing it.

## Creates a special "matrix" object that can store its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize cached inverse as NULL
    
    # Function to set a new matrix and reset cached inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Function to get the current matrix
    get <- function() x
    
    # Function to set the cached inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # Function to get the cached inverse
    getInverse <- function() inv
    
    # Return a list of the above functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, it retrieves it from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Check if inverse is already cached
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)  # Return cached inverse
    }
    
    mat <- x$get()  # Get the matrix
    inv <- solve(mat, ...)  # Compute the inverse
    x$setInverse(inv)  # Cache the inverse for future use
    inv  # Return the inverse
}
