## Put comments here that give an overall description of what your functions do

## This function caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    
    ## This part assigns the values to x & m (if m is not assigned)
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
    
    ## This part calls the inverse functions and create a list
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the matrix

cacheSolve <- function(x, ...) {
    
## This part verifies if cache data is available and uses it
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
## This part inverses the matrix that is available and outputs it
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
