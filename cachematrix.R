## The two functions take a matrix and check if the
## inverse has already been calculated. If it has
## the functions get the inverse from the cache.
## If it hasn't the functions calculate the inverse,
## store the matrix in the cache and retur the
## result.

## The first function creates a list with a function to:
## 1. Set the valule of the matrix,
## 2. Get the value of the matrix,
## 3. Set the value of the inverse,
## 4. Get the vlaue of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## 1. Set the valule of the matrix
        set <- function(y) {   ,
                x <<- y
                m <<- NULL
        }
        ## 2. Get the value of the matrix,
        get <- function() x  
        ## 3. Set the value of the inverse,
        setinverse <- function(inverse) m <<- inverse
        ## 4. Get the vlaue of the inverse.
        getinverse <- function() m
        ## Create list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function checks if the inverse
##  of the matrix is in the cache, and returns the inverse
## from the cache if it is. If the cache is empty the 
## function calculates the inverse, saves it to the cache
## and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Check cache
        m <- x$getinverse()
        if(!is.null(m)) {
                message("retrieving data that is cached")
                return(m)
        }
        ## Get the Matrix
        data <- x$get()
        ## calculate inverse
        m <- solve(data, ...)
        ## Save to cache
        x$setinverse(m)
        ## Return Matrix
        m
}
