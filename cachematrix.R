## Set of functions to create matrix-like objects that can cache its inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL                                  # Initialise the inverse variable
     set <- function(y) {                         # Function to set the matrix value
          x <<- y
          inv <<- NULL
     }
     get <- function() x                          # Function to get the matrix value
     setinv <- function(inverse) inv <<- inverse  # Function to set the inverse matrix value
     getinv <- function() inv                     # Function to get the inverse matrix value
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()                            # Tries to retrieve the inverse matrix value
     if(!is.null(inv)) {                          # If a value is found, it is returned
          message("Getting cached data")
          return(inv)
     }
     mat <- x$get()                               # Otherwise retrieves the matrix value
     inv <- solve(mat, ...)
     x$setinv(inv)
     inv
}
