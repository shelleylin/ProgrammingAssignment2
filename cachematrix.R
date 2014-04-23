## 2 functions to create a special object that stores a matrix and cache its inverse

## makeCacheMatrix returns a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
      x<<-y
      i<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## cacheSolve calculates the inverse of the matrix created above. 
## But it first checks to see if it has been calculated. If so, it skips the calculation.
## If not, it does the calculation and updates the cache.

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
