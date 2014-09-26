## R Programming Assignment 2: Lexical Scoping
## Assignment: Caching the Inverse of a Matrix 

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## get the value of the matrix 
    get <- function() x
    ## set the value of the inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    ## get the value of the inverse of the matrix
    getinverse <- function() inv
    ## return list 
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
        if(!is.null(inv)) {
            message("Getting cached data...")
            return(inv)
        } 
      data <- x$get()
      ## To calculate the inverse matrix using the standard function solve()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}

## For example
## Create matrix
M <- cbind(c(1,0,1),c(0,1,2),c(0,0,1))
m = makeCacheMatrix(M)
m$get()
##      [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    1    2    1
cacheSolve(m)
## No cache in the first run
##      [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]   -1   -2    1
cacheSolve(m)
## Getting cached data...
##      [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]   -1   -2    1
## By definition of the inverse matrix: A*A^(-1)=E, where A-any matrix, and E-identity. Check:
mi <- cacheSolve(m)
## Getting cached data...
mi%*%M
##      [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1

