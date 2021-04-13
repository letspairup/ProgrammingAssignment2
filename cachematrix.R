## create matrix and calculate inverse of matrix once !!

## create list with getter/setter function for metrix and metrix inverse

makeCacheMatrix <- function(m = matrix())
{
    mi <- NULL
    set <- function(givenM) {
        m <<- givenM
        mi <<- NULL
    }
    get <- function() m
    setinverse <- function(givenMi) mi <<- givenMi
    getinverse <- function() mi
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
# rertun cached inverse if availabble otherwise calcuate and cache it
cacheSolve <- function(m, ...) {
    mi <- m$getinverse()
    if(!is.null(mi)) {
        message("getting cached inverse")
        return(mi)
    }
    mi <- solve(m$get(), ...)
    m$setinverse(mi)
    mi
}
