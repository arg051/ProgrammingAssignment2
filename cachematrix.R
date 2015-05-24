## there are two functions here:  
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix or returnes cached inverse of the matrix 


## the makeCacheMatrix function creates a list containing a function to
## get - return the value of the matrix
## setSolve - cache the inverse of the matrix 
## getSolve - return cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        s <- NULL                          ## reset cached inverse of the matrix
        
        get <- function() {                ## function get -  get matrix        
                x
        }
        
        setSolve <- function(m) {         ## function setSolve - cache the inverse of the matrix
                s <<- m
        }
        
        getSolve <- function() {           ## function getSolve - return cached matrix
                s
        }
        
        list(   get = get,                 ##create and return the list containing the functions 
                setSolve = setSolve,
                getSolve = getSolve)
        
}




## the cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix 
## if the inverse has already been calculated, the cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        
        
        s <- x$getSolve()                               ##get the cached value
        
        if(!is.null(s)) {                               ##there is a cached value, no need to compute 
                
                message("getting cached matrix")          ##notify that the cached result is returned
                
                return(s)                               ##return the cached result
        }
        
        data <- x$get()                                 ##get the matrix
                                         
        s <- solve(data, ...)                           ##compute the inverse of the matrix  
        
        x$setSolve(s)                                   ##cache the inverse of the matrix
        
        s                                               ##return the result 
        
}
