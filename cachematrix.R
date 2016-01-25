## Put comments here that give an overall description of what your
## functions do

## This function converts square matix x to "special" matrix suitable
## for cached calculations. It adds "methods" to work with given matrix

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <- function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setsolve <- function (solved) m<<-solved
  getsolve <-function() m
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## This function implements cached approach to calculation of inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setsolve(m)
  m
}
