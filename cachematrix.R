## Below are two functions that are used to create a special object that stores
## a matrix and caches its inverse, using the <<- operator

## 'makeCacheMatrix' creates a special "matrix", which is really a list
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}

## 'cacheSolve' calculates the inverse of the special "matrix" created with the
## function defined above. It first checks to see if the inverse has already
## been calculated. If it has not been calculated, it will compute the inverse 
## and store the inverse in the cache using the setinverse function. Otherwise, 
## it will return the inverse from the cache and will skip the computation.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
