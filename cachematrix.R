## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will
## not discuss here)

## `makeCacheMatrix` is a function that creates a special "matrix" object
## that can cache its inverse.

## Assumption: The input matrix is invertible matrix
makeCacheMatrix <- function(x = matrix())  
{

m<-NULL
  set_mat <-function(y=matrix())
  {
    x <<- y
    m<<-NULL
  }
  get_mat <- function()x
  
  set_matinv <- function(solve) m<<-solve(x)
  get_matinv <- function() m
  
  list(set_mat=set_mat, get_mat=get_mat,
       set_matinv=set_matinv,get_matinv=get_matinv)
}


## `cacheSolve` is a function that computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
##`cacheSolve` retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    
	## Assumption: The input matrix is invertible matrix
	
		 m<-x$get_matinv()
  if(!is.null(m))
  {
        message("Getting Cached Inverted Matrix Data")
  return(m)
  }
  data<- x$get_mat()
  m<-solve(data, ...)
  x$set_matinv(m)
  m
}



