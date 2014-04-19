#------------------------------------------------------------------------
#--- Function: makeCacheMatrix
#--- Description: This function adds a matrix to the cache and provides
#---              accessor functions to set and get the inverse of the
#---              cached matrix. If inv is NULL then the inverse has not
#---              been computed.  If inv is NOT NULL then the inverse has
#---              been computed and cached.
#------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL
    set <- function(y)  #- Method for changing the cached matrix
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list (set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

#-----------------------------------------------------------------------
#--- Function: cacheSolve
#--- Description: Checks to see if the inverse is in the cache and
#---              returns it if it is. Otherwise it calls the built in
#---              solve() function to compute the inverse of the matrix
#---              and saves the inverse to the cache.
#------------------------------------------------------------------------
cacheSolve <- function(x, ...)
{
    inv <- x$getinverse()  #- Get the inverse from the cache
    if (!is.null(inv))     #- Check to see if the inverse has already been computed
    {
        message("getting cached data")
        return(inv)
    }
    
    #- If the inverse has not been computed, then compute, store to the cache,
    #- and return the matrix inverse.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}