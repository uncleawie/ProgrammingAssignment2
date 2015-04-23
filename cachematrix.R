## cachematrix.R 

#################################################################
## Name:
## cacheMatrix.R - a module for caching matrix solutions

#################################################################
## Synopsis:

## my_matrix = matrix(rnorm(100),nrow = 10, ncol = 10)
## my_cm     = cacheMatrix(my_matrix)
## solution <- cacheSolve(cm)  # same as solve(my_matrix)
## again    <- cacheSolve(cm)  # now quickly retrieved from cache
##

#################################################################
## Description:

## Matrix inversion is a computationally expensive operation 
## so code that repeatedly requires the solution to a matrix
## inversion may run slowly and inefficiently.

## This module provides a method of creating cacheMatrix objects
## from a native R matrix object and cacheSolve(obj,...) can now
## be used to prevent repeat calls to the expensive solve() function.

## The object provides four getter/setter methods used internally
##  obj$get         - gets the original matrix data
##  obj$set         - sets a new matrix and forgets the cached solution
##  obj$get_inverse - gets the solution to solve(obj$get) or NULL 
##                    if not yet compouted
##  obj$set_inverse - sets the solution to solve(obj$get) if/when found

#################################################################
## Bugs
##
## This module only caches the first solution regardless of the other
## arguments provided to cacheSolve. 
##
## m <- matrix(rnorm(9),nrow =3, ncol=3)
## cm <-  makeCacheMatrix(m) 
## inv_m <- cacheSolve(cm)       # Returns m^-1
## ident_m <- cacheSolve(cm, m)  # Returns m^1  Wrong!!!!  
##                               # This should give identify matrix    

#################################################################
## Methods:

##  makeCacheMatrix  - Object creation method. 
##  Input:  a native R matrix object 
##  Output: a cacheMatrix object.
##  Example: 
##       cm <- makeCacheMatrix(matrix(rnorm(100), nrow=10, ncol=10))

makeCacheMatrix <- function(my_matrix = matrix()) {
  my_inverse  <- NULL  # the inverse if/when we calculate it
  
  ## obj$set is a function that allows us to update my_matrix
  set   <- function( new_matrix ) {
    # Did we get passed a matrix ?
    if( is.matrix(new_matrix) ) {
      ## Good now we can update my_matrix to be the new_matrix 
      my_matrix   <<- new_matrix  
      ## We don't know the inverse any more so set it to NULL
      my_inverse  <<- NULL  
    } else {
      ## Bother the user gave us a non matrix object
      stop('"set" requires a matrix object')
    }
  }
  
  # Use the set method to ensure the user gave us an R matrix object
  set(my_matrix)
    
  ## obj$get is a function that returns the current my_matrix
  get <- function() my_matrix 
  
  ## obj$set_inverse is a funtion to update my_inverse_of_matrix
  set_inverse <- function(new_inverse) {
    if( is.matrix(new_inverse) ) {
      my_inverse <<- new_inverse  
    } else {
      stop('set_inverse requres a matrix object')
    }
  }
  ## obj$get_inverse returns the current my_inverse matrix 
  get_inverse <- function() my_inverse
  
  # The object is a list of the four functions as well as this
  # calling environment within this makeCacheMatrix constructor
  self <- list(
    set = set,
    get = get,
    set_inverse = set_inverse,
    get_inverse = get_inverse
  )
  #return this object
  self
}


## cacheSolve 
##  solve the inverse matrix of a give cacheMatrix object
##  Input: a cacheMatrix object
##  Output: a native matrix object
## 
##  Example: 
##      cm <- makeCacheMatrix(1:20,ncol=5,nrow=4)
##      solution <- cacheSolve( cm )

cacheSolve <- function(cm, ...) {
  # do we have the inverse computed yet?
  my_inverse <- cm$get_inverse()
  if( !is.null(my_inverse) ) {
    message("getting cached data")
    return(my_inverse)
  }
  # We don't have a cached inverse. We will need to compute it 
  my_matrix  <- cm$get()              # load the matrix
  my_inverse <- solve(my_matrix,...)  # solve the matrix
  cm$set_inverse(my_inverse)          # store the solution
  my_inverse                          # return the solution
}
