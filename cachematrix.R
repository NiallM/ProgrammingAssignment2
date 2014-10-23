## makeCacheMatrix takes in a matrix value
##and stores it in cache
##then cacheMatrix reads the cache and returns the inverse
##of the matrix

## makeCacheMatrix takes in a matrix 
##an example matrix would be
##> z <- (c(1,2 ,2,1), nrow=2, ncol=2)
## this matrix can then be passed into the function as a variable
##> z2 <- makeCacheMatrix(z)
## the passed in matrix is then stored in the cache
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInv <- function(solve) m <<- solve
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## this function reads in the x parameter
## and then looks up the cache to find the matrix variable
##> cacheSolve(z2)
## the inverse of the matrix is then returned
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
      message("getting cached matrix")
      return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setInv(m)
    m
}
