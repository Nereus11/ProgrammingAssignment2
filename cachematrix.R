

## This function reads a matrix using makeCacheMatrix() and caches the value of m to be NULL. 
## it then establishes a list of functions for use in cachesolve() below
## to manipulate which environment m stored and called from.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        
        setmatrix <- function(solve) m<<- solve
        
        
        getmatrix <- function() m
        
        list(set = set, get = get, setmatrix = setmatrix,getmatrix = getmatrix) 
}

## This function calls on the makeCacheMatrix environment where it retrieves the value of m, 
## and if m == NULL computes the inverse for x and sets the cache for m as the inverse matrix.
## If m !== NULL the function prints "getting cached data" to the console and returns the value of m.

cachesolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                
                message("getting cached data")
                return(m)
                
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}