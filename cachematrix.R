## Well, if I was doing this for myself, I would have made everything easier. 
## But I will try to wright my script exectly like in the instruction

## The first function, makeCasheMatrix creates a special object, which is really a list containing a function to
## set the value of the matrix and clear inmat, where inveted matrix value is stored
## get the value of the original matrix
## set the value of the inverted matrix
## get the value of the inverted matrix
## everything right like in the example. just names a litle bit changed 

makeCacheMatrix <- function(x = matrix()) {
    inmat <- NULL
    set <- function(y) {
        x <<- y
        inmat <<- NULL
    }
    get <- function() x
    setinv <- function(inmatrix) inmat <<- inmatrix
    getinv <- function() inmat
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The following function calculates the inverted matrix If it has not beed calculated before 
## or get it's value from cache
## With x$getinv it  checks to see if the inverted matrix has already been calculated. 
## If so, it gives us the invered matrix from cache and skips the computation. 
## Otherwise, it calculates it and save in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    inmat <- x$getinv()
    if(!is.null(inmat)) {
        message("getting cached data")
        return(inmat)
    }
    data <- x$get()
    inmat <- solve(data, ...)
    x$setinv(inmat)
    inmat
}