## "matrix" object creation for inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse_ <- NULL
    set <- function(x_) {
        x <<- x_;
        inverse_ <<- NULL;
    }
    get <- function() return(x);
    setinv <- function(inv) inverse_ <<- inv;
    getinv <- function() return(inverse_);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## inverse of the object created by makeCacheMatrix
cacheSolve <- function(x, ...) {
    inv_ <- x$getinv()
    if(!is.null(inv_)) {
        message("Assigning cached data")
        return(inv_)
    }
    data_ <- x$get()
    inv_ <- solve(data_, ...)
    x$setinv(inv_)
    return(inv_)
}

