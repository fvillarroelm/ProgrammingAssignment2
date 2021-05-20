## makeMatrix will return a list of functions that
## set and get the matrix, and set and get the inverse
## of the matrix.

## cacheSolve will compute the inverse of the matrix
## only if it has not been yet calculated. If it has
## been calculated, it will return the cached (saved)
## inverse matrix.
## Note: the cache will be stored only if "makeMatrix"
## function is identical between caches.

## "x" is a vector that will be transformed to a 
## symmetric matrix. "i" will store the inverse of
## the matrix. Output is a list of functions.
## Lexical scoping is fundamental in order to
## understand what the function does.

makeMatrix <- function(x=matrix()) {
  i <- NULL
  x <- matrix(x,nrow=length(x)/2,ncol=length(x)/2)
  set=function(y){
    x <<- y
    i <<- NULL
  }
  get<-function() x
  setinverse<-function(solve) i <<- solve
  getinverse<-function() i
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}
## This will compute the inverse if it has not been
## computed for the "x" object yet. If it has been
## already computed (and "x" object has not been changed)
## it will return the cached inverse of the matrix.

cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)){
          message("getting cached data")
          return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setinverse(i)
        i
}

# Test: is it well computed the inverse?
Matrix=makeMatrix(1:4)
cacheSolve(Matrix)
Matrix=makeMatrix(c(-2,1,1.5,-0.5))
cacheSolve(Matrix)
# Test: does the cache works?
Matrix=makeMatrix(1:4)
cacheSolve(Matrix)
cacheSolve(Matrix)
