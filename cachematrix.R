## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.  The
## following functions compute and cache the inverse of an invertible matrix.

## makeCacheMatrix accepts parameter x which should be a square invertible
## matrix and returns a list containing functions to
##   1. set the matrix
##   2. get the matrix
##   3. set the inverse
##   4. get the inverse
makeCacheMatrix <- function(x = matrix()) {
        ## initialize 'cache' attribute for inverse
        inv <- NULL
        ## function definitions
        set <- function( aMatrix ) {
                x <<- aMatrix  ## assign to parent environment
                inv <<- NULL   ## ditto for 'cache' attribute
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve accepts parameter x which should be the output of makeCacheMatrix
## and returns the inverse of the matrix originally passed to makeCacheMatrix.
## Note: before computing the inverse of the matrix, the function first
## checks to see if the inverse has already been computed.  If so, it gets
## result and skips the calculation.  Otherwise, it computes the inverse
## and sets this value in the cache.

cacheSolve <- function(x, ...) {
        ## First, check the cache
        inv <- x$getInverse() 
        if (!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        ## Must not be in the cache, compute the inverse and store
        x$setInverse(solve(x$get()))
        inv
}


## Sample/Testing run:
## set.seed(352635)
## r = rnorm(10*10)
## data = makeCacheMatrix(matrix(r, nrow=10))
## cacheSolve(data)
## ## second time called should print "getting cached data." before returning matrix
## cacheSolve(data)

