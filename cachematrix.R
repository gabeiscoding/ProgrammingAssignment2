## Provies a system to cache the computation of an inverse of matrixes.
## 1) Wrap your matrix with makeCacheMatrix
## 2) Pass wrapped variable into cacheSolve. Only the first time will the inverse
##    computation be computed. The inverse is returned.
##
## Example: 
## > hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## > h8 <- hilbert(8); h8
## > h8c <- makeCacheMatrix(h8)
## > cacheSolve(h8c) #work done
## > cacheSolve(h8c) #cache result returned

## Wrap a matrix in a cached inverse.
## Use $get() to get the matrix, and $get.inverse() to get teh cached inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set.inverse <- function(inverse) i <<- inverse
    get.inverse <- function() i
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}

## Computes the inverse of a matrix wrapped by makeCacheMatrix
## Only the first call will call solve() on the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$get.inverse()
    if(!is.null(i)) {
        #message("getting cached data")
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$set.inverse(i)
    i
}
