## set of functions to illustrate Lexical Scoping in R
## makeCacheMatrix - function whhich creates a matrix with supporting functions
## cacheSolve - function which computes and caches the inverse of matrix created
##      through earlier call of makeCacheMatrix

## Flow of execution is
## a <- create a square matrix
## aa <- makeCacheMatrix(a)
## cacheSolve(aa) # first call will compute inverse
## cacheSolve(aa) # all subsequent calls will return cached copy with the indicative message

## makeCacheMatrix
##    takes a matrix as parameter and forms constructs to
##    provide functionality of set, get, set_inverse and get_inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function (y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    set_inverse <- function(c_inv) inv_x <<- c_inv
    get_inverse <- function() inv_x
    list(set = set, get = get, set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## cacheSolve
##    takes the matrix already formed using earlier call to makeCacheMatrix
##    then computes inverse of the matrix on the first call and caches the result
##    returns the cached version of the inverse of the matrix for all subsequent
##    calls to cacheSolve on that particular matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$get_inverse()
    if(!(is.null(inv_x))) {
        message("Returning cached inverse")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data)
    x$set_inverse(inv_x)
    inv_x
}
