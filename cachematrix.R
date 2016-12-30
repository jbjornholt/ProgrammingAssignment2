## The function stores a matrix in a list object, caching it, it's functions
## and the inversion (if created) in memory.
## It's provided with setters and getters to access or change the content.
## Upon change of the matrix (calling 'set()'), the inversion is deleted.

## The function returns a List object with the functions and cached matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ##Caching the matrix in the 'set' function and resetting the inverse.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ##The rest of the setters and getters with standard functionality
    get <- function() x
    setinv <- function(inversion) inv <<- inversion
    getinv <- function() inv
    
    ##Returning a list object with the 4 functions with their respective
    ##cached objects in them.
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    ##Checking whether the inverse is already created, and returning it
    ##if it's the case.
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    ##Creating inversion of matrix and caching it in the makeCacheMatrix object
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
