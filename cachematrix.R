## Caching the inverse of a matrix rather than compute it repeatedly

makeCacheMatrix <- function(x = matrix()) {  
    
    ## Creates a special "matrix" object that can cache its inverse
    x_inv <- NULL

    set <- function(y) {
        ## set the value of the matrix
        x <<- y
        x_inv <<- NULL
    }

    ## get the value of the matrix
    get <- function() x

    ## set the value of the inverse matrix
    setinv <- function(inv) x_inv <<- inv
    
    ## get the value of the inverse matrix
    getinv <- function() x_inv

    list(set = set, get = get, setinv = setinv, getinv = getinv)

} 

cacheSolve <- function(x, ...) { 
    
    ## Return a matrix that is the inverse of 'x' 
    x_inv <- x$getinv()
    
    ## Checks if the inverse matrix has already been calculated
    if(!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }

    ## solve the value of the inverse matrix if no cache found
    data <- x$get()
    x_inv <- solve(data, ...)
    x$setinv(x_inv)
    x_inv

} 
