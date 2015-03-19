###################################
##  Author : Arun Kumar Madas
##  Date : 03/16/2015
##  Coursera : R Programming
####################################

## -------------------------------------------------------------------------------------------
## Problem statement : 
##      (a) Compute Inverse matrix of a given matrix
##      (b) Leverage Cache mechanism to reduce computation (if inverse was already computed) 
##          by Using environment specific variables for caching
##
## Usage (How to run the program):--
#
#         >source("cachematrix.R")
#         > m<-matrix(c(-1,-2,1,1),2,2)
#         > x<-makeCacheMatrix(m)
#         > x$get()
#               [,1] [,2]
#         [1,]   -1    1
#         [2,]   -2    1
#         > inv<-cacheSolve(x)
#         > inv
#               [,1] [,2]
#         [1,]    1   -1
#         [2,]    2   -1
#         > inv<-cacheSolve(x)
#         getting cached matrix data
#         > inv
#                [,1] [,2]
#         [1,]    1   -1
#         [2,]    2   -1
#         > 
#         > 
#         > #http://www.wikihow.com/Find-the-Inverse-of-a-3x3-Matrix
#         > 
#         > m<-matrix(c(1,0,5,2,1,6,3,4,0),3,3)
#         > x<-makeCacheMatrix(m)
#         > x$get()
#               [,1] [,2] [,3]
#         [1,]    1    2    3
#         [2,]    0    1    4
#         [3,]    5    6    0
#         > inv<-cacheSolve(x)
#         > inv
#               [,1] [,2] [,3]
#         [1,]  -24   18    5
#         [2,]   20  -15   -4
#         [3,]   -5    4    1
#         > inv<-cacheSolve(x)
#         getting cached matrix data
#         > inv
#              [,1] [,2] [,3]
#         [1,]  -24   18    5
#         [2,]   20  -15   -4
#         [3,]   -5    4    1
#
## -------------------------------------------------------------------------------------------


### --------------------------------------------------------------------------------------------
## "makeCacheMatrix" is a function that takes an argument x (which is a matrix)
##              and defines set of 4 methods, setter, getter, setinverse, getinverse
##              
##              (a) set - method sets the matrix local argument to x defined in makeCacheMatrix
##              (b) get - method gets the matrix x (which was set using (a) method)
##              (c) setinverse - method used to cache the computed inversematrix
##              (d) getinverse - method used to read the cached inversematrix (if not computed/present it will return null)
### -------------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
        
        # inverseMatrix is variable that will contain computed inverse matrix of matrix x
        # initialize the inverseMatrix to NULL 
        inverseMatrix <- NULL
        
        # setter function used to initialize a new matrix for computation of its inverse.
        # local argument y will be used to set the value of x defined in parent function
        # initialize the  parent inverseMatrix to null that will later trigger computation of inverse matrix
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        
        ## getter function used to get given matrix for computation of inverse
        get <- function() { 
                x
        }
        
        ## After computing the inverse of the matrix, set the value into the parent variable
        ## inverseMatrix. Preserve the value of computed inverseMatrix
        setinverse <- function(invMatrix) {
                inverseMatrix <<- invMatrix
        }
        
        ## getting inverse matrix that was set using setinverse method
        getinverse <- function() {
                inverseMatrix    
        } 
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## "cacheSolve" will check if for a given matrix x, was the inverse matrix already computed and present in cache?
##              if yes, retrieve it from the cache and return it without computing again
##              if no, compute the inverse matrix and set it inside the cache (to avoid re-computation later)

cacheSolve <- function(x, ...) {
        
        ## for a given matrix x, get its inverse by calling getinverse() method, 
        #  (first checking in the cache if it was already computed)
        invMatrix <- x$getinverse()
                
        ## if the value of invMatrix is not null, return it (from cache) without recomputing all 
        ## over again.
        if(!is.null(invMatrix)) {
                message("getting cached matrix data")
                return(invMatrix)
        }
        
        ## if the value of invMatrix is null, i.e., inverse was not computed before.
        ##  get the value of input matrix
        data <- x$get()
        
        ##Compute the inverse of the matrix
        invMatrix <- computeRInverse(data)
        
        ##Store the computed inverse matrix in the cache
        x$setinverse(invMatrix)
        
        #return the computed inverse matrix
        return(invMatrix)
}

## "computeRInverse" will compute the inverse of a given matrix
## Assumes that matrix inputed is always invertible
computeRInverse <- function(x) {
        
        #compute the inverse of the matrix using solve() function in R
        return(solve(x))
}
