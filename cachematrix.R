## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#--------------------------------
#The makeCacheMatrix creates a special “matrix” object that can store its 
#inverse in a hidden environment.

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse <- function() m <<- inv(x)
  getinverse<-function() m

}


## Write a short comment describing this function

#CacheSolve function computes the inverse of the matrix if it is not cached, 
#or retrieves it from the cache if it is.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m<-x$getinverse
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get
  m <- inv(data, ...)
  x$setinverse(m)
  m
}
