## Week 3 Part 1 Course: R Programming
## Name: Santiago Armida
## Date: 2015-03-21
## Function: makeCacheMatrix 
## Creates a special matrix object that can cache its inverse
##  
makeCacheMatrix <- function(x=matrix()) {
     inversex <- NULL
     print(environment())
     cenv <- environment()
     print(parent.env(cenv))
     inm <- function(y) {
         x <<- y
         inversex <<- NULL
     }
     res <- function() x
     inmatrix <- function(solve) inversex <<- solve(x)
     outmatrix <- function() inversex
     inenv <- function() environment()
     list (inm=inm, res=res, outmatrix=outmatrix, inmatrix=inmatrix,inenv=inenv)
 }	
## Function: cacheSolve 
## Computes the inverse of the special matrix returned by
## makeCacheMatrix.R. If the inverse has already been calculated
## then the cacheSolve should retrieve the inverse form the cache.
##

cacheSolve <- function(x, ...) {
  inenv <- x$outmatrix()
  if (!is.null(inenv)) {
    message("getting matrix cached data")
    return(inenv)
  }
  matdata <- x$res()
  inm <- solve(matdata, ...)
  x$outmatrix(inm)
  inm
}
