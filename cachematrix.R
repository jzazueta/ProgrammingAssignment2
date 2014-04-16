## The next two functions are used to reduce computational cost when the inverse
## of a matrix needs to be computed and used repeatedly during a procedure.
 

## MakeCacheMatrix produces a special object that stores a matrix and caches its
## inverse. 
##

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## cacheSolve retrieves the cached value from 
## makeCacheMatrix and checks for an existing cache. If the cache is not null, it
## retrieves that value. If its null, it performs the calcualtion.
## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
