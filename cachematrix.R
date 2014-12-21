## Program to return a cached solution to inverse of a matrix
## The first time the script is loaded and cacheSolve function is executed 
## the inverse of a matix is calculated and stored. The next time cacheSolve
## is called on the same matrix it returns the saved inverse of the matrix 

## make a matix which can save the value of the calculated inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculate and return the inverse of a matrix or if the matrix has a 
## pre-cached solution, return that as the result.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)
    }
    message("Calculating and caching inverse")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

