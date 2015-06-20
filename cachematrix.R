## R Programming JHU - Coursera
## Programming Assignment 2
## Joy Flowers   June 19, 2015

makeCacheMatrix <- function(x = matrix()) {
        if(nrow(x) != ncol(x)) {
                stop("Matrix must be square. Enter the same number of rows as columns")
        }        
        invmatrix <- NULL                       #invmatrix will hold the inverse matrix
        setmatrix <- function(y= matrix()) {    #Establishes the matrix
                if(nrow(y) != ncol(y)) {
                        stop("Matrix must be square. Enter the same number of rows as columns")
                } 
                x <<- y                         #Lexical Scoping in action
                invmatrix <<- NULL
        }
        getmatrix <- function() x               #Returns the matrix that was entered
        setinverse <- function(inversem) invmatrix <<- inversem     #Establishes the inverse matrix
        getinverse <- function() invmatrix      #Returns th inverse of the matrix that was entered
        list(setmatrix = setmatrix, getmatrix = getmatrix,       #Sets up a list of functions
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function cacheSolve retrieves cached data (the inverse matrix) if it is available.
## If the inverse matrix has not yet been computed, it computes it based on the matrix given 
## in the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmatrix <- x$getinverse()
        if(!is.null(invmatrix)) {               # If already cached, do not compute, but get cached data
                message("getting cached data")
                return(invmatrix)
        }
        data <- x$getmatrix()                   # Retrieves the input matrix
        d <- det(data, ...)
        if (d == 0) {                           # Checks to make sure determinant isn't zero, otherwise no inverse exists
                message("Determinant is ",d)
                stop("An inverse matrix is invalid if the determinant is zero. Please go back and enter a matrix where the determinant is not zero")
        }
        invmatrix <- solve(data, ...)           # If inverse matrix was never computed, do it now
        x$setinverse(invmatrix)                 # Sets the inverse matrix
        invmatrix
}
