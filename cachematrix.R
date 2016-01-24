## Cache the inverse of a matrix, so we don't need to calculate repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #browser()
  m<-NULL
  set<-function(y){
    x <<-y
    m<<-NULL
  }
  get<-function() x
  
  setinverse <- function(inverse) m<<-inverse
  
  getinverse<-function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated, it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
  
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached inverse")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  message("setting inverse")
}
