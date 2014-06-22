## This programming assigment contains two functions, namely "makeCacheMatrix" and "cacheSolve".
## Overall goals is to return the cache of an inverse of a matrix to avoid computing it repeatedly.

#---------------------------------------------------------------------------
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse:
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInv <- function(solve) m <<- solve
    getInv <- function() m
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

#----------------------------------------------------------
## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}

#----------------------------------------------------------
# end-of-file