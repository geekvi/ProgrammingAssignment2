## Put comments here that give an overall description of what your
## functions do
## This file contains two functions makeCacheMatrix and 
##

## Write a short comment describing this function
## 
##  This function has four operations set,get, setmatrix, getmatrix
##  It will use a seapate environement to cache   
##  setmatrix will use solve() and will need an argument
##
makeCacheMatrix <- function(x = matrix()) {
  mat<-NULL
  set<-function(y){
    x<<-y
    mat<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) mat<<- solve
  getmatrix<-function() mat
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)  

}


## Write a short comment describing this function
## 
## cacheSolve have to use spacial object produced
## by makecacheMatrix as an argument
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m  
  
}
