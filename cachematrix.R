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
    ## set the value of the matrix
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    ## get the value of the matrix
    get <- function() x
    
    ## set the value of the inverse matrix
    setinverse <- function(inverse) inv <<- inverse
    
    ## get the value of the inverse matrix
    getinverse <- function() inv
    
    ## creating the list to be returned
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
    ## checking if there is cached data
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    ## otherwise, we compute the inverse using the solve function
    data <- x$get()
    inv <- solve(data, ...)
    
    ## and stores the computed value in the cache
    x$setinverse(inv)
    inv
}
