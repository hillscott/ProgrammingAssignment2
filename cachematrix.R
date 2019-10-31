## cacheMatrix
## By: Ian Reynolds
## These functions are used together to calculate AND cache the inverse of a matrix

## makeCacheMatrix take in a matrix and returns a list object to be used by cacheSolve
## NOTE: The matrix passed in MUST BE an invertible matrix.
## More info here on what that means: 
# https://www.khanacademy.org/math/precalculus/x9e81a4f98389efdf:matrices/x9e81a4f98389efdf:intro-to-matrix-inverses/v/singular-matrices
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve takes in an object (list) created by makeCacheMatrix
## It will then EITHER use the setinverse / solve function to find the inverse
## OR
## If the object passed to cacheSolve was already used (and an inverse matrix exists)
## It will simply returned the cached inverse matrix object
## Use these functions to speed up looped matrix inversions
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## runInversions takes in a matrix and runs through an inversion / caching test
## NOTE: the matrix must be invertible. 
## More info on what that means here:
# https://www.khanacademy.org/math/precalculus/x9e81a4f98389efdf:matrices/x9e81a4f98389efdf:intro-to-matrix-inverses/v/singular-matrices
runInversions <- function(x){
        # x = an invertible matrix
        mtxObj <- makeCacheMatrix(x)
        mtx <- cacheSolve(mtxObj)
        # Print the first cell
        print(mtx[1,1])
        # A message should be generated here from the cacheSolve function to inform you of the caching usage
        mtx <- cacheSolve(mtxObj)
        # Print the first cell (from cache)
        print(mtx[1,1])
}
