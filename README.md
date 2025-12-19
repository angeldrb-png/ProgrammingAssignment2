## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the cache as NULL, indicating no inverse is calculated yet.
    inv <- NULL
    
    # Setter function: sets the matrix and invalidates the cache.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Getter function: returns the original matrix.
    get <- function() x
    
    # Setter for the inverse: stores the calculated inverse in cache.
    setinverse <- function(inverse) inv <<- inverse
    
    # Getter for the inverse: retrieves the cached inverse.
    getinverse <- function() inv
    
    # Return a list of the four functions, which defines the special "matrix" object.
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix`.
## If the inverse has already been calculated (and the matrix is unchanged),
## it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    # First, check if the inverse is already cached.
    inv <- x$getinverse()
    
    # If cached value exists, return it and skip computation.
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    
    # If not cached, get the original matrix.
    data <- x$get()
    
    # Compute the inverse using the standard `solve` function.
    inv <- solve(data, ...)
    
    # Store the computed inverse in the cache.
    x$setinverse(inv)
    
    # Return the inverse.
    inv
}
