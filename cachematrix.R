## ProgrammingAssignment2
## In this Programming Assignment will take advantage of the scoping rules of the R 
## language and how they can be manipulated to preserve state inside of an R object.
## Computation intensive actions can be minimized by making use of unchanged data from 
## the cache, rather that computing the action everytime.

## The function (makeCacheMatrix) creates a special "martix", which can set or get a 
## martix, as well as set or get the inverse of a invertible, square matrix.

makeCacheMatrix <- function(x = matrix()) {
    k <- NULL
    setmatrix <- function(q) {
        x <<- q
        k <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(solve) k <<- solve
    getinverse <- function() k
    list(setmatrix = setmatrix,
         getmatrix = getmatrix, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## This function (cacheSolve) return a matrix that is the inverse of 'x'. It will however 
## first test it can find the already computed inverse in the cache before solving it again. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    k <- x$getinverse()
    if(!is.null(k)) {
        message("getting from the cache")
        return(k)
    }
    data <- x$getmatrix()
    k <- solve(data) 
    x$setinverse(k)
    k
    
}



