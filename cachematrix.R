

## Matrix inversion main purpose is to cache the inverse of a matrix.
## The following two functions are used to 
## cache the inverse of a matrix.
## makeCacheMatrix creates a list that will contain the functions: it will 
## set/get the value of the matrix, and it will set and/or gee the value of 
## inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve returns the inverse of a matrix.
## The assumpution with this function is that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

