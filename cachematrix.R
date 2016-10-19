## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  matInv<-NULL
  set <-function(y){
    x<<-y
    matInv <-NULL
  }
  get <-function(y) x
  setinverse<-function(inverse)matInv <<-inverse
  getinverse<-function() matInv 
  list(set=set,get=get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  matInv<-x$getinverse()
  if(!is.null(matInv)) {
    message("getting cached data")
    return(matInv)
    
  }
  mat<-x$get()
  matInv <-solve(mat,...)
  x$setinverse(matInv)
  matInv
}
