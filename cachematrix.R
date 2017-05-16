## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse rather than compute it repeatedly. 
##Here is a pair of functions that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL
 set<-function(y){
     x <<- y
     inv<<-NULL
 }
 get<-function()x
 setinverse<-function(inverse) inv<<- inverse
 getinverse<-function()inv
 list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
     }


## cacheSolve: This function computes the inverse of the special "matrix" returned by the makeCacheMatrix function above. 
## If the inverse has already beeen calculated (and the matrix has not changed), then the cacheColve function should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns it's inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv<-x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    data<-x$get()
    inv<-solve(data, ...)
    x$setinverse(inv)
    inv
}

