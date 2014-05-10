## The following two functions demonstrate how to leverage the scoping rules of the 'R'
## language to cache potentially time-comsuming computations.

## An example illustrating this is taking the inverse of a matrix and caching it for 
## look up a later time, assuming the contents of the matrix are unchanged. This avoids 
## unnecessary recomputation of the inverse

## USAGE:

##      source("cachematrix.R")
##      create an invertible matrix as follows: >mat<-diag(5)
##      create a 'special' matrix using the makeCacheMatrix function:
##              mat_sp<-makeCacheMatrix(mat)
##      calculate inverse by using the cacheSolve function:
##              inverse<-cacheSolve(mat_sp)
##      >doing a >inverse will print the inverse 
##      now compute the inverse a second time, you will see that the value will be fetched from the cache
##      and the message 'getting cached data' displayed on the console

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<-y
                m<<-NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, 
             getsolve = getsolve)
}

## The "cacheSolve" function calculates the inverse of the special "matrix"
## created by the "makeCacheMatrix" function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets its value  in the cache via the `solve` function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matt <- x$get()
        m <- solve(matt,...)
        x$setsolve(m)
        m
}

        

