## With the help of the makeCacheMatrix and cacheSolve functions we can get
## the inverse of an invertible matrix. If the inverse has been already 
## calculated then the inverse is returned from the cached inverse and the
## inverse is not calculated again.

## makeCacheMarix creates a special object which stores a matrix and it 
## can store the inverse of the matrix in a cache.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getMatrix <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)

}


## cacheSolve returns the inverse of the input matrix. If the inverse of 
## this matrix has been already calculated then the cacheSolve returns 
## the inverse from the cache. If the inverse hasn't been calculated then 
## this function calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    matrix <- x$getMatrix()
    inv <- solve(matrix, ...)
    x$setInverse(inv)
    inv
}
