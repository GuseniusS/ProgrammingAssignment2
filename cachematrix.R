## Together these functions provide a method of storing cached data so upon 
## so that data need not be recreated when used successively.

## Function 'makeCacheMatrix' caches an input matrix and optionally its inverse. 
## Once Stored 'makeCacheMatrix' provides the ability for 'cacheSolve' 
## (and potentially other functions)to retrieve or overwrite the stored 
## matrix and/or its inverse. 

makeCacheMatrix <- function(x = matrix()) {
      ## Create a cached version of 'x'
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        
        get <- function() x
        setInvMat <- function(invMat) m <<- invMat
        getInvMat <- function() m
        list(set = set, get = get,
             setInvMat = setInvMat,
             getInvMat = getInvMat)            
}

## Function 'cacheStore' checks to see if an inverse matrix has yet 
## been generated.  If it has it retrieves and outputs it.  If it has 
## not, the original cached matrix is retrieved from 'makeCacheMatrix',
## its inverse is calculated, cached back to 'makeCacheMatrix', and the 
## inverse is returned to the calling function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        m <- x$getInvMat()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInvMat(m)
        m 
}
