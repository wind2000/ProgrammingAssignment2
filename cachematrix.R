## Creates a special matrix with inverse caching functionality.

makeCacheMatrix <- function(x = matrix()) {
        ## stores the inverse of the input matrix.
        invcache <- NULL
        
        ## sets the new matrix. Cache will be invalidated.
        set <- function(mat) {
          x <<- mat
          invcache <<- NULL
        }
        
        ## gets the stored matrix.
        get <- function() x
        setinverse <- function(invmat) {
          invcache <<- invmat
        }
        
        ## gets the cached inverse of the matrix.
        getinverse <- function() invcache
        
        ## returns the special matrix
        list(set = set, get =  get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates inverse of the special matrix returned by makeCacheMatrix.
## Inverse of matrix already calculated will be cached and retrieved by this
## method, if input matrix is not changed.

cacheSolve <- function(x, ...) {
        ## gets cached data.
        inversemat <- x$getinverse()
        
        ## check whether the inverse of the input matrix is already cached.
        if(!is.null(inversemat)) {
                message("getting cached data")
                return(inversemat)
        }
        
        ## data not cached.
        ## calculate inverse and cache it in special matrix.
        data <- x$get()
        
        inversemat <- solve(data, ...)
        
        x$setinverse(inversemat)
        
        ## returns the inverse of the matrix.
        inversemat
}
