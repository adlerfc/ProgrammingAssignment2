## The two following functions store a numeric matrix and caches its inverse

##The makeCacheMatrix function returns a list of functions to get and set, 
##the data matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) I <<- inverse
    getinverse <- function() I
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function returns the inverse of the matrix if its already calculated. 
## Otherwise it computes the inverse and stores it.

cacheSolve <- function(x, ...) {
    I <- x$getinverse()
    if(!is.null(I)){
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data)
    x$setinverse(I)
    I
        ## Return a matrix that is the inverse of 'x'
}
