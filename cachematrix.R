#-------
# Since the Matrix inversion is usually a high cost computacion, there are advantages 
# in caching the inverse of a matrix rather than compute it repeatedly. 
# The next two functions are used to cache and use the inverse of a matrix.
#-------


#-------
# This function creates a list of functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#-------
# This function calculates the inverse of the matrix. It uses cache in order avoid 
# the matrix inversion computation desnecessarily. So, it first checks if
# the matrix inverse has already been computed. If so, it gets the result from cache,
# skiping the computation. If not, it then calculates the matrix inverse update the cache.

# This function assumes the matrix is invertible.
cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
