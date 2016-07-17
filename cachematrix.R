
## Programm Assignment No. 2 for the course "R Language"
## Coursera, John Hopkins University


## Create matrix object with getters/setters
## The only field is matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    mx_inv <- NULL
    set <- function(y){
        x <<- y
        mx_inv <<- NULL
    }
    get <- function() x
    setinv <- function(slv) mx_inv <<- slv 
    getinv <- function() mx_inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Takes an instance of the matrix object as argument and returns 
## its inverse.
## The inverse is taken from cache if it is not empty, otherwise
## the function calculates the inverse programmatically and 
## assignes it to the mx_inv variable.

cacheSolve <- function(x, ...) {
    mx_inv <- x$getinv()
    if(!is.null(mx_inv)) {
            message("getting cached data")
            return(mx_inv)
    }
    data <- x$get()
    mx_inv <- solve(data, ...)
    x$setinv(mx_inv)
    ## Return a matrix that is the inverse of 'x'
    mx_inv
}
