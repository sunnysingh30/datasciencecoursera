# Matrix inversion is usually a costly computation and there may be
# some benefit to caching the inverse of a matrix rather than compute
# it repeatedly (there are also alternatives). 

# This page consists of below 2 pair of functions that cache 
# the inverse of a matrix:
# -- makeCacheMatrix() -- This function creates a special "matrix" object 
#   that can cache its inverse
# -- cacheSolve() -- This function computes the inverse of the special matrix
#   returned by the above function. If the inverse has already been calculated 
#   and matrix has not changed, then cacheSolve() should return the value from 
#   the cache

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(mat_ = matrix()) {
    inverse <- NULL
    set <- function(mat){
        mat_ <<- mat
        inverse <<- NULL
    }
    get <- function() mat_
    setInv <- function(inv) inverse <<- inv
    getInv <- function() inverse
    
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
# should retrieve the inverse from the cache.
cacheSolve <- function(mat_fun, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- mat_fun$getInv()
    if(!is.null(inv)){
        message("Getting cached value(s).")
        return(inv)
    }
    mat_data <- mat_fun$get()
    inv <- solve(mat_data, ...)
    mat_fun$setInv(inv)
    return(inv)
}
