## The main objective of two functions in this script is to 
## calculate the inverse of a matrix and keep a cache of the 
## calculation such that if a pre-calculated matrix is called 
## then instead of full calculation the cached result is used


##################################################
## NOTE: This R script assumes that the user is ##
## entering an invertible (non-singular) matrix ##
##################################################


################################################
############      NEW FUNCTION      ############
################################################
## This function creates a special"matrix" 
## object that can cache the inverse of a matrix
## NOTE: After you define a square matrix
## you need to send that matrix to this makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


################################################
############      NEW FUNCTION      ############
################################################
## This function calculates the inverse of a matrix 
## but checks that is whether it has already been 
## calculated earlier or not
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
