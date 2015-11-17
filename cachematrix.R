## Lexical Scoping Exercise
## Takes an input matrix called 'blah'
## One function caches the matrix inverse once it has been determined
## The second function returns the matrix inverse in one of two ways:
## either by finding it in the cache or by working it out directly if it isn't cached

## create a random 2x2 matrix
set.seed(3)
blah <- matrix(runif(4, 1, 10), 2, 2)

## Function changes the format of a matrix such that if its inverse is determined,
## the inverse is stored with the matrix
## Otherwise, the inverse element of the reformatted matrix remains blank
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# 'foo' is the reformated version of matrix 'blah'
foo <- makeCacheMatrix(blah)

## Function takes the reformatted matrix and outputs it's inverse
# either it looks up the inverse if it has already been cached
# or it works out the inverse itself
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        print("using cached data") # let's me know that the inverse is coming from the cache
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

cacheSolve(foo) # gives the inverse of the matrix
cacheSolve(foo) # the fuxn has been run once and therefore the inverse is cached
                # this time the message "using cached data" is printed followed 
                # by the inverse of the matrix
