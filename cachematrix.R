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
        get <- function() {
                x
        }
        
        ## stores the inverse of the matrix in cache.
        setinverse <- function(invmat) {
          invcache <<- invmat
        }
        
        ## gets the cached inverse of the matrix.
        getinverse <- function() {
                invcache
        }

        ## returns the special matrix
        list(set = set, 
             get =  get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates inverse of the special matrix returned by makeCacheMatrix.
## If the inverse of matrix is already calculated ( and the input matrix has 
## not changed), it will be cached and retrieved by this method.

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

## Usage:
# m <- makeCacheMatrix(matrix(1:4, 2, 2))
# cacheSolve(m)
## output:
##    [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
# cacheSolve(m)
## output:
## getting cached data
##    [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5