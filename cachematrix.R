## Fucntions to get the inverse of a matrix with a cached value

## Function that create an special object that contains the value of a matrix and expose 
#  the functionality to calculate the solve just if the value of the matrix has changed

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## Function that gets the inverse of a matrix from the cache version if possible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
