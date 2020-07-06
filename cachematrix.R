## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {                                             ## define the argument with default mode of "matrix"
## This function creates a special "matrix" object that can cache its inverse
        i <- NULL                                                                       ## initialize i as NULL; will hold value of matrix inverse 
        set <- function(y){                                                             ## define the set function to assign new
                x  <<- y                                                                ## value of matrix in parent environment
                i <<- NULL                                                              ## if there is a new matrix, reset i to NULL
        }
        
        get <- function()x                                                              ## define the get function - returns value of the matrix argument
        setInverse <- function(inverse) i <<- inverse                                   ## assigns value of i in parent environment
        getInverse <- function()i                                                       ## gets the value of i where called
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)    ## you need this in order to refer  to the functions with the $ operator
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)){
                message("Getting cached data")
                return(i)
        }
        
        matrix <- x$get()
       i <- solve(matrix, ...)
        x$setInverse(i)
        i
}
