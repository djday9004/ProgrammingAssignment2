## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function stores the inverse of a matrix if it has not been stored previously

makeCacheMatrix <- function(x = matrix()) {
        ## initialize starting values
        inverse <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Insert the current value of x into the variable get
        get <- function() x
        
        ## cache the inverse of x into setinv
        setinv <- function(solve) inverse <<- solve
       
        ## get the inverse from the cached value
        getinv <- function() inverse
        
        ## update the list of current values
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Write a short comment describing this function
## this function computes the inverse of a matrix if it has not already been stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Load the existing function value of getinv into inverse
        inverse <- x$getinv()
       
        ## Check to see if inverse is empty, if mot use the current value of inverse
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        
        ## Use the data from the get() function as an argument for solve for the inverse of x
        data <- x$get()
        
        ##compute the inverse of data
        inverse <- solve(data)
        
        ## cache inverse for later use
        x$setinv(inverse)
        
        ##return the latest value of inverse
        inverse
}
