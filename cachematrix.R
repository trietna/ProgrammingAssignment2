## Put comments here that give an overall description of what your
## functions do

## This is a function that make a list consist of 4 objects: get, set, getinverse, setinverse.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
  set<-function(){
    x<<-y
    inv<<-NULL
  }
  get<-function(y) x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This is a function to compute a inverse matrix if it doesn't exist.
## Otherwise, it returns inverse one from cache matrix data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data.")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
}
