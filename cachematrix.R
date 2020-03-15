## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
    ## Initialize
    i <- NULL
    ## Set the matrix
    set <- function( matrix ) {
            m <<- matrix
    }     
        
    ## Get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }
    ## Set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    ## Get the inverse of the matrix
    getInverse <- function() {
        i
    }   
     ## Return methods above
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated , then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    ## Return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
    ## Get the matrix
    data <- x$get()
    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data
    ## Set the inverse to the object
    x$setInverse(m)
    ## Return the matrix
    m
}
