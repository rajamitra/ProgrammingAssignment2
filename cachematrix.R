## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y){
    
    x <<- y
    m <<- NULL
    
  }
  get <- function() x
  
  setinv <- function(inv) m <<- inv
  
  getinv<- function() m
  
  list( set= set, get =get ,
        setinv = setinv,
        getinv = getinv)
  

}


## This function first tries to get the inverse matrix of 
## the matrix passed as argument and retrn it. 
##If the inverse is not caluculated.
##It will calculate the inverse and return it. 
## This code assumes that the matrix passed in is a square matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m<- x$getinv()
  
  if(!is.null(m)){
    
    message("getting cached data")
    return(m);
    
  }
  
  data <- x$get()
  
  m <- solve(data)
  
  x$setmean(m)
  
  m
  
}
