## Fucntions to get the inverse of a matrix with a cached value

## Function that create an special object that contains the value of a matrix and expose 
#  the functionality to calculate the solve just if the value of the matrix has changed

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## function that sets the value of the matrix
    ## if the value of the matrix change the inverse must be recalculated
    ## then the cache must be cleaned
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get the value of the matrix
    get <- function() x
    ## set the value of the inverse of the matrix
    setsolve <- function(solve) m <<- solve
    ## get the value of the inverse of the matrix
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## Function that gets the inverse of a matrix from the cache version if possible

cacheSolve <- function(x, ...) {
    ## Returns the inverse of the matrix
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    ## If the inverse is not cached then calculates the inverse and sets the cache value
    x$setsolve(m)
    m
}
