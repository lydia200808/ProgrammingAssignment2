## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        # initialize the variable storing 'inverse' of x 
        i <- NULL
        
        # method to set/reset an input matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # method to get an input matrix
        get <- function() x
        
        # method to set the inverse of matrix
        setinverse <- function(inverse) i <<- inverse
        
        # method to get the inverse of matrix
        getinverse <- function() i
        
        # return a list of all methods
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned 
## by `makeCacheMatrix` above. If the inverse has already been 
## calculated (and the matrix has not changed), then `cacheSolve` should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        # fetch inverse of matrix 'x' which is input of function makeCacheMatrix
        i <- x$getinverse()
        
        # return the inverse if it's already computed
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # get input matrix 'x'
        TempMat <- x$get()
        
        # compute the inverse of 'x'
        i <- solve(TempMat, ...)
        
        # cache this inverse in third element of z
        x$setinverse(i)
        
        # return inverse matrix
        i
}


