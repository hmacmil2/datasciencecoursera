## The following two functions allow a user to supply a matrix, store that matrix, and set, store,
## Or compute a solution to a matrix.

##      The makeCacheMatrix function takes a matrix as an argument and creates a list of 
##      functions that can set or return the stored matrix, or can set or return a solution 
##      to the matrix (inverse matrix) that can be called.
    
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL 
    set <- function(y) { 
        x <<- y 
        inverse <<- NULL 
    }
    get <- function() x 
    setinverse <- function(newinverse) inverse <<- newinverse
    getinverse <- function() inverse 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}

#The cacheSolve function calculates the inverse of the matrix created with the above function,
#but first checks if an inverse has already been calculated and returns it from the cache if it has. 

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached matrix solution")
        inverse
    } else {
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
    }
}