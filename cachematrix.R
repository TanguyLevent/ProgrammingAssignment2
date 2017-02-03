## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        # holds the cached value or NULL if nothing is cached
        # initially nothing is cached so set it to NULL

        i <- NULL
        
        # store a matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # returns the stored matrix
        get <- function() x
         
        # cache the given argument 
        setinv <- function(inverse) i <<- inverse
                
        # get the cached value
        getinv <- function() i
                
         # return a list. Each named element of the list is a function
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        i <- x$getinv()
       
        # if a cached value exists return it
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        # return the inverse
        i
}
