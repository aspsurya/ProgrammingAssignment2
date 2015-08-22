## This program describes "How to Cache a Matrix Inversion using " << " & 
## solve() function.

## makeCacheMatrix(): This function creates a specila matrix object that 
## can cache its inverse.
makeCacheMatrix <- function(mat = matrix()) {
        # @mat  : a square invertible matrix
        # return: a list containing functions to
        #           1. set the matrix
        #           2. get the matrix
        #           3. set the inverse
        #           4. get the inverse
        #         this list is used as the input to cacheSolve()
        
        inverse = NULL
        # Set the matrix
		set     = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                mat <<- y
                inverse <<- NULL
        }
		
		# Get the matrix
        get    = function() mat
		
		# Set the inverse of the matrix
        setinv = function(inverse) inverse <<- inverse 
		
		# Get the inverse of the matrix
        getinv = function() inverse
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve(): This function computes the inverse of the matrix returned 
## by makeCacheMatrix() function.  If the inverse has already been calculated
## (and the matrix has not changed), then this function retrieves the inverse
## from the cache directly.
cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
        ## @mat: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = mat$getinv()
        
        # if the inverse has already been calculated (caching)
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse (non-caching)
        mat.data = mat$get()
        inv = solve(mat.data, ...) # inverse calc function
        
        # sets the value of the inverse in the cache via the setinv function.
        mat$setinv(inv)
        
        return(inv)
}
