## Functions in this file allows to compute inverse form of the matrix
## with caching. Preserves unnecessary computation.


## Creates a wrapper around the matrix object for storing 
## its inverse form cached
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv
        list(set = set, get = get, 
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Lazy calculate matrix inverse function which uses cache
## argument must be matrix wrapper mentioned above
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$set_inverse(inv)
        inv       
}
