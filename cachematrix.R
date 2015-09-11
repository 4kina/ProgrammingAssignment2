#set the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #get the value of the matrix
        get <- function() x
        #set the value of the inverse matrix
        setinv <- function(solve) inv <<- solve
        #get the value of the inverse matrix
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        message(" not getting cached data")
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
# how to use the functions in R-console:
# 1) m<-makeCacheMatrix(any matrix)
# 2) cacheSolve(m) 