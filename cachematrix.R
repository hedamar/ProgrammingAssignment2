## The first function (makeCacheMatrix) creates a special matrix which is actually a list containing four functions:  
## One to set the value of the input matrix in the parent environment; 
## One to retrieve the input matrix; 
## One to set the inverse of the matrix (in the cache or parent environment); 
## One to retrieve this cached inverse. 
## The second function then checkes to cache to see if there is a (non-null) inverse already stored there
## If so, then it reports the cached inverse. If not, it inverts the input matrix and uses the setinv function
## from the list returned by the first function to set this inverse in the parent environment

## Write a short comment describing this function -- Please see above

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y     # this sets whatever is inputed into the "set" function as the parent environment's x (i.e input matrix)
        m <<- NULL  # this confirms that when a new matrix is inputted, the cached inverse gets cleared (i.e. returned to null)
    }
    
    get <- function() x # this reports what the input matrix (i.e. x in the parent environment) is
    
    setinv <- function(inv) m <<- inv # This resets m (i.e. the result) to be the calculated inverse (as opposed to NULL). 

    getinv <- function() m # This retrieves the m from the parent environment to check what the cached result is (NULL vs. the already cached inverse)
    
    list(set = set, get = get, setinv = setinv, getinv = getinv) # Create the list of the four functions discussed above
}

## Please see above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()     # Check to see if the inverse is already calculated and cached or not. If not, report cached inverse.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # If the cache is NULL, then (34) call the data (i.e. input matrix), 
    # (35) calculate the inverse (notice that the inverse --m-- is currently in the function and not parent environment),
    # (36) use setinv to make this calculated inverse cached in the parent environment
    data <- x$get() 
    m <- solve(data, ...)  
    x$setinv(m)            
    m
}
