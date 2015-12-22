## function makeCacheMatrix stores four functions:
## get returns the vector that is stored in the main function.
## set stores the vector in the main function.
## getInverse returns the inverse that is stored in the main function.
## setInverse stores the vector in the main function.

## This function will create a matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## initialize 'inverted' as NULL
        inverted <- NULL
        set <- function(y) {
                 x <<- y
                ## reset 'inverted' as NULL when new matrix is loaded.
                inverted <<- NULL
        }
        ## get returns the stored matrix
        get <- function() x
        ##setInverse stores the inverse of the matrix
        setInverse <- function(solve) inverted <<-solve
        ##getInverse returns the stored inverse
        getInverse <- function() inverted 
       ## store functions in a list.
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function will check to see if the inverse of the matrix has been created already.
## If it has been created, it will return the cached version.  if not, it will calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## retrieve cached Inverse value
        cached <- x$getInverse()
        
        ##check to see if there's a cached value for the inverse.
        if(!is.null(cached)) {
                ## if not null, return cached version.
                message("Cache exists, retrieving.")
                return(cached)
        }
        ##if null, get the matrix, invert it, and store the cached value.
        message("No Cache, calculating.")
        data <- x$get()
        inverted <- solve(data, ...)
        x$setInverse(inverted)
        return(inverted)
}

