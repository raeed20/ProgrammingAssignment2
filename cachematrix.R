## the first function creates a litst contatining a function to: 
## set the matrix entries, get them, set the inverse of a matrix, and get the it

## the second function returns the inverse of a matrix that was created using the first function
## if the inverse was calculated in prior to the call, it returns the cached inverted matrix
## if not, it calculates it and sets it in the cache


makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
