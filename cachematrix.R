# Programming Assignment 2 
# Author : Devesh Lohumi


# Information : For caching we use the <<- operator which can be used to 
# assign a value to an object in an environment that is different from the current environment

# this function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  inverse <- NULL
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set=set,get=get,setinverse=setinverse,
       getinverse=getinverse)
}

# this function computes the inverse of a special matrix returned by the 
# function above, if the inverse has already been calculated then the cacheSolve
# function retrieves the inverse from the cache

cacheSolve <- function(x, ...) 
{
    # Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv))
    {
      message("getting cached matrix")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
