## This purpose of the functoin is the to calculate the inverse and cache the
## result and return from result from the calculation or cache if already been
## calculated before.  Assume that the matrix is always invertible

## Write a short comment describing this function
## a function that create a special "matrix", which is a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    ## special "matrix"
    inv_x <- null
    set <- function(y) {
        x <<- y
        inv_x <<- null
    }
    get <- function() x
    setinverse <- function(inverse) inv_x <<- inverse
    getinverse <- function() inv_x
    list (set = set, get = get, 
          setinverse = setinverse,
          getinverse = getinverse)
}



## This function will return the inverse of matrix from cache (of the same matrix)
## when available otherwise calcuate and store the cache of matrix for future
## result

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinverse()
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data, ...)
    x$setinverse(inv_x)
    inv_x
}
