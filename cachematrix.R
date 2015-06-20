## R Programming JHU - Coursera
## Programming Assignment 2
## Joy Flowers   June 19, 2015
## This program illustrates lexical scoping within the R programming language.
## The user calls the makeCacheMatrix function, passing a matrix to it, such as: 
## inmatrix <- makeCacheMatrix(matrix(c(2,-1,1,4,1,4,1,-1,0), nrow=3, ncol=3))
## The function makeCacheMatrix creates a list containing four functions:
## setmatrix, getmatrix, setinverse, getinverse which can all be accessed 
## and run at the console. 
## The program also illustrates how to cache the inverse of the matrix, as the computation
## can be costly in terms of performance. Once the inverse of the matrix has been
## computed one time, it is cached into memory, so that the next time it is simply
## retrieved from cache. The cacheSolve function checks to see if the inverse has
## been stored in cache. If so, it retrieves the information from cache, otherwise it 
## makes the inverse matrix computation. It can be called as cacheSolve(inmatrix)
## The function getmatrix returns the matrix , such as inmatrix$getmatrix()
## The function setmatrix sets up a new matrix
## The function getinverse returns the inverse of the matrix
## The function setinverse assigns the inverse matrix and is called by cacheSolve.
## The program checks to make sure the matrix is square, and the determinant is not 0
## which are necessary criterion in order for the matrix to have an inverse.
## Example: Once inmatrix <- makeCacheMatrix(matrix(c(2,-1,1,4,1,4,1,-1,0), nrow=3, ncol=3)) is called, then
## inmatrix$getmatrix() returns the following:
##         [,1] [,2] [,3]
##  [1,]    2    4    1
##  [2,]   -1    1   -1
##  [3,]    1    4    0
##  
## cacheSolve(inmatrix) returns the inverse of the matrix such as:
##           [,1] [,2] [,3]
##     [1,]   -4   -4    5
##     [2,]    1    1   -1
##     [3,]    5    4   -6
## After the inverse has been set (within the cacheSolve function),
## the function inmatrix$getinverse() returns the inverse.
## If the inverse matrix has been cached, a message will say it is retrieving from cache.
## The matrix elements and dimensions can be reset using inmatrix$setmatrix(matrix(c(2,3,4,3),2,2))
## and the whole process can start again, for example using inmatrix$getmatrix() or cacheSolve(inmatrix)
## 

## The function makeCacheMatrix is passed a matrix, and it creates a list containing four functions:
## setmatrix, getmatrix, setinverse, getinverse which can all be accessed 
## and run at the console. 

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
