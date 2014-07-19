## This function will take a matrix, and calculate the 
## inverse of that matrix. Once the inverse is 
## calculated, it will be cached and used the next time the
## function is called so that it does not need to be calculated again.


## This function creates a special vector which is a list
## that contains a function to set the value of the
## vector, get the value of the vector, set the value
## of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function calculates the inverse of the vector
## created above. If the inverse has already been 
## calculated, it gets the value from the cache.
## If not, it will calculate the inverse and then
## store it in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}