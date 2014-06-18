#a set of utility functions to provide caching for matrix 
#inversion operations

# Create a special matrix object that can
# cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    csolve <- NULL
   
    set <- function(y) {
            x <<- y
            csolve <<- NULL
    }

    #returns matrix
    get <- function() x
 
    #use to set cached solution
    setsolved <- function(s) csolve <<- s
    #get uached solution
    getsolved <- function() csolve

    #return cache matrix as a list
    list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}

#computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    #look for cached value
    csolve <- x$getsolved()

    #if you found cached value, return it...
    if(!is.null(csolve)) {
        message("getting cached data")
        return(csolve)
    }

    #otherwise calculate the data and cache it before returning
    data <- x$get()
    s <- solve(data, ...)
    x$setsolved(s)
    s
}
