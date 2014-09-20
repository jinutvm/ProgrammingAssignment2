## To utilize caching while calculating inverse of a function to avoid performing
## same calculate again in case of no change in matrix.

## Write a short comment describing this function
##The first function, makeVector creates a special "vector", which is really 
##a list containing a function to
## 1. Set the value of martix
## 2. Get the value of martix
## 3. Set the inverse of matrix
## 4. Get the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y){
       x<<-y
       inverse<<-NULL
     }
     get<-function() x
     setinverse <- function(inv) inv<<-inverse
     getinverse <- function() inv
     list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


##CacheSolve calculates the inverse of special matrix created above. It checkes
##whether inverse is already calculated. If so use the inverse, else calculate
##inverse and set the inverse using setinverse function.

## chk whether Get inverse is not NULL, then retrieve from cache
## else get the matrix using get() and calculate using solve fun
## and store inverse in cache using setinverse fun

cacheSolve <- function(x, ...) {
    invk <- x$getinverse()
    if(!is.null(invk)){
      message("getting cached data")
      return(invk)
    }
    temp <- x$get()
    invk<-solve(temp,...)
    x$setinverse(invk)
    invk
}
