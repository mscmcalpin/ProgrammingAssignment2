## This pair of functions caches the inverse matrix
##
## makeCacheMatrix takes a matrix and outputs a cachable matrix
## cacheSolve takes the result of makeCacheMatrix and either calculates the inverse matrix or returns the 
##      cached inverse matrix
##
##
## Example of call:
##      mat = rbind(c(1, -1/4), c(-1/4, 1))
##      a <- makeCacheMatrix(mat)
##      cacheSolve(a)
## Alternative callings:
##      mat = rbind(c(1, -1/4), c(-1/4, 1))
##      cacheSolve(makeCacheMatrix(mat))
##      OR
##      cacheSolve(makeCacheMatrix(rbind(c(1,-1/4),c(-1/4,1))))

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # Return the value the matrix that is passed in
    get <- function() x
    
    ## Cache the inverse matrix
    setsolve <- function(solve) m <<- solve
    
    ## Return Cache value or null if nothing has been cached.
    getsolve <- function() m
    
    ## retunr the internal functions definitions set, get, setSolve, getSolve when called with no paramters
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##      If the inverse has already been calculated (and the matrix has not changed), then the cachesolve wiil 
##      retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Attempt to the matrix first from cache and assign to local m     
    m <- x$getsolve()
    
    ## If cache is not empty return cache value
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    ## put matrix in local variable and solve
    data <- (x)$get()  
    m <- solve(data)
    ## this in essence is doing m <- solve(x)
    
    # Store matrix in cache
    x$setsolve(m)
    
    ## Return inverse matrix
    m
}
