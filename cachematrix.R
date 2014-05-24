#These functions invert the given matrix, first searching the cache to optimize the computation time 

#This function takes a matrix and returns a list of functions.  
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
		# set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x # just get the matrix
        setSolve <- function(solve) m <<- solve #solve function inverts the matrix
        getSolve <- function() m # get the supplied matrix inverse
		# a list of functions
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

#This function  returns the inverse of 'x' for use it in 'setSolve' function . If the matrix has inverted, returns it from memory.
cacheSolve <- function(x, ...) {
        m <- x$getSolve() #try to get the inverse matrix from the cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) #invert the matrix
        x$setSolve(m) #assigns m as the inverse matrix of x
        m
}
