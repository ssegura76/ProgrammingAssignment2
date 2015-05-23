## The function makeCacheMatrix creates a matrix wich is a list that
## contains a function that set the value of the matrix, get the value
## of the matrix, set the value of the inverse and get the value of
## te inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve calculates the inverse of the matrix created in the 
## makeCacheMatrix function and checks first if the inverse has already
## been calculated. It gets the invese from the cache and skips the computation
## otherwise if calculates the inverse from the data and sets the value of the 
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
