## Coursera, R Programming
## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. The assignment is to write a pair of functions that cache 
## the inverse of a matrix. 

## Put comments here that give an overall description of what your
## functions do

## functions:
##  (1) makeCacheMatrix(x = matrix()), which creates
##      a caching matrix
##  (2) cacheSolve(x, ...), which returns the inverse
##      of a caching matrix
##  (3) testCacheMatrix(), which tests the above two
##      functions on a pre-defined (hardcoded) matrix.
##      The function demonstrates how to call the functions
##      as well. To execute simply call:
##          testCacheMatrix()
##      with no parameters
##
##  Note that cacheSolve() assumes that the matrix is
##  invertible always.


## Write a short comment describing this function
## makeCacheMatrix :: creates a special "matrix", which is really a list containing functions to
##    1. set the value of the matrix 
##    2. get the value if the matrix 
##    3. set the value of the inverse
##    4. get the value of the inverse 
##    
##    makeCacheMatrix() :: creates object containing an empty matrix, use set(<matrix>) to set the value
##    makeCacheMatrix(<matrix>) :: creates object initialized with <matrix> 
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## cached inverse matrix
        ## set new matrix
        set <- function(y) {
                x <<- y           
                i <<- NULL  ## clear cached inverse         
        }
        get <- function() x  ## return enclosed matrix and helps to check if it is already cached
        ## cache inverse
        setInverse <- function(inverse) i <<- inverse
        ## get cached inverse
        getInverse <- function() i
        
        ## return list of functions for manipulating matrix and its cached inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)        
}


## Write a short comment describing this function
## cacheSolve :: returns inverse of the special "matrix" object created with makeCacheMatrix
##               if the inverse was already computed, returns pre-computed value
##               Note : cashSolve allows the caller to pass additional parameters to r funcion solve
##                      calling solve on same matrix but different additional parameters might produce
##                      different results, which is not accounted for by cashSolve. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()  
        
        ## check if object x has cached the inverse value
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## inverse hadn't been cached yet, compute it
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)    ## cache the inverse value
        i    ## Note : returnued value is a matrix
        ## Return a matrix that is the inverse of 'x'
}
# test
# m <- makeCacheMatrix(matrix(c(0, 2, 1, 0, 5, 4, 2, 3, 4), nrow = 3, ncol = 3))
# cacheSolve(m)
# cacheSolve(m)
