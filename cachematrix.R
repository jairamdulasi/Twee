## The below functions are used to obtain the inverse of a matrix by creating a object that stores a matrix
## and calculate its inverse.


## This function creates a matrix object that can cache it inverse.

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


## This function computes the inverse of the matrix created from the above function. If the inverse has already
## been calculated already, then result is obtained from the cache.


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
