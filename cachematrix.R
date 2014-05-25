## These routines creates a matrix which can cache its inverse to avoid re-computing
##      the matrix which has already been calculated.

##  Routine to create a cache for input matrix x and results using the super assignment '<<-' operator
##  N.B. There is no error checking to make sure the matrix is properly sized, which could be added.
makeCacheMatrix <- function(x = matrix()) {
        invMatrix  <- NULL
        set  <- function(y){
                x <<- y
                invMatrix <<- NULL 
        }
        get  <- function() x
        setInv  <- function(solve) invMatrix  <<- solve
        getInv  <- function() invMatrix
        list(set= set, get = get, 
             setInv = setInv, 
             getInv = getInv)

}


## Routine computes the inverse of the input matrix. If the inverse has already been calculated (and is unchanged),
## then the cacheSolve retrieves the inverse from the cache and does not recalculate inverse
cacheSolve <- function(x, ...) {
        invMatrix  <- x$getInv()
        if (!is.null(invMatrix)){
                message("Getting data from cache")
                return(invMatrix)
        }
        data  <- x$get()
        invMatrix  <- solve(data, ...)
        x$setInv(invMatrix)
        invMatrix
}
