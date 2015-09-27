## Note: Matrix should be a square invertible matrix for these functions to work

##The makeCacheMatrix will read a matix object and provides a list where in 
##you can set & get a matrix object and its inverse. Cachesolve reads a list object
## formed by makeCacheMatrix retrieve inverse of a matric object from cache,
##if its already in DRAM of R or else it will be computed and sets the inverse.


## Reads a matrix object and formslist of function to set & get matrix and its 
##inverse

makeCacheMatrix <- function(m = matrix()) {
    
    I <- NULL
    
    set <- function(n= matrix())
    {
      m <<- n
      I <<- NULL
    }
    get <- function() m
    setinverse <- function(inv) I <<- inv
    getinverse <- function() I
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Reads a list object from above function and retrieves inverse from cache if
##its not null(already computed) otherwise computes the inverse and sets it
##to respective matrix.

cacheSolve <- function(x, ...) 
{
## Return a matrix that is the inverse of 'x'
    I <- x$getinverse()
    if (!(is.null(I)))
    {
        message("Cached Inverse")
        return(I)
    }
    
    matX <- x$get()
    I <- solve(matX)
    x$setinverse(I)
    I
    
}
