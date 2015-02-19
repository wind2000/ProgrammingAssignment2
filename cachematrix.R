## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invcache <- NULL
        
        set <- function(mat) {
          x <<- mat
          invcache <<- NULL
        }
        
        get <- function() x
        setinverse <- function(invmat) {
          invcache <<- invmat
        }
        
        getinverse <- function() invcache
        
        list(set = set, get =  get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversemat <- x$getinverse()
        if(!is.null(inversemat)) {
                message("getting cached data")
                return(inversemat)
        }
        data <- x$get()
        inversemat <- solve(data, ...)
        x$setinverse(inversemat)
        inversemat
}
