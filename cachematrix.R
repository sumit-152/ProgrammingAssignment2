## Below two functions are created as part of coursera assignment 2.
## Main goal is to check and understand about lexical scoping. 
## makeCacheMatrix :This function retrieve inverse of the matrix 
## from caches rather than calculating each time.This will save the time-
## consuming computations.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL    ## Initialization of Inv as null to hold inverse matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x ## define the get fucntion to return matrix value
    
    setInverse <- function(inverse) inv <<- inverse 
                                ## assigns value of inv in parent environment
    getInverse <- function() inv
                                ## gets the value of inv where called
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## cacheSolve: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
    
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {   ## Check the cached data for Inv 
        message("getting cached data")
        return(inv)        ## Return the Inv from cached 
    }
    data <- x$get()
    inv <- solve(data, ...) ## Function call to matrix inverse
    x$setInverse(inv)
    inv
}