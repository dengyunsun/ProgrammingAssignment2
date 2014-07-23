
## Function makeCacheMatrix makes a matrix object that can cache its inverse.  
## Function cacheSolve computes the inverse of the special matrix returned by "makeCacheMatrix".
## If the inverse has already been calculated and the matrix has not been changed,
## the cacheSolve retrieves the inverse from the cache.


makeCacheMatrix <- function( mat = matrix() ) {
	
    inv <- NULL   ## Initialize the inverse property

    ## Method to set the value of the matrix:
    set <- function(matrix) {
            mat <<- matrix
            int <<- NULL
    }

    ## Method the get the value of the matrix
    get <- function() { 	
    	return mat   ## Return the matrix
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {       
        return inv   ## Return the inverse property
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}




cacheSolve <- function(x, ...) {   
    mat <- x$getInverse()   ## Return a matrix that is the inverse of 'x'

    ##  Return the inverse if it is already set
    if( !is.null(mat) ) {
            message("getting cached data")
            return(mat)
    }

    
    data <- x$get()    ## Get the matrix from our object
    
    mat <- solve(data) %*% data     ## Calculate the inverse using matrix multiplication

    
    x$setInverse(mat)    ## Set the inverse to the object
    
    return mat    ## Return the matrix
}
