

#### 1. file desp ############################################################
#  date��20170823
#  note:this is the homework of  <R programming> week 3.

#  desp: it Cach the Inverse of a Matrix.it mainly use the <<- operator .
         # it contains two function :makeCacheMatrix,cacheSolve

rm(list = ls())
##########################################################！#
# function name :makeCacheMatrix
# function desp :creates a special "matrix" object that can cache its inverse.
# arguments:matrix
# return : list contains"set,get,setInverse,getInverse"
#########################################################！#

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y){
        x <<- y
        inverse_matrix <<- NULL
    }
    get <-function() x
    setInverse <- function(inverse){
        inverse_matrix <<- inverse
    } 
    getInverse <- function() inverse_matrix
    list(set = set,get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
##########################################################！#
# function name :makeCacheMatrix
# function desp :computes the inverse of the special "matrix" returned by 
    # makeCacheMatrix . If the inverse has already been calculated 
    # (and the matrix has not changed), then the cachesolve should retrieve 
    # the inverse from the cache.
# arguments:x is a list contains"set,get,setInverse,getInverse"
# return : Return a matrix that is the inverse of 'x'
##########################################################！#
cacheSolve <- function(x, ...) {
        
    inverse_matrix <- x$getInverse()
    if (!is.null(inverse_matrix)){
        message("getting cached inverse matrix.")
        return(inverse_matrix)
    }
    tmp_matrix <- x$get()
    inverse_matrix <- solve(tmp_matrix,...)
    x$setInverse(inverse_matrix)
    inverse_matrix
}

####2. test functions above########################################


a <-makeCacheMatrix(matrix(c(1,1,2,3),2,2))

cacheSolve(a)   # inverse matrix isn't in cash, calculate inverse matrix 
cacheSolve(a)   # get the invert matrix in cache
a$get()         # get inputed matrix ,proved that it haven't be changed

# set a new matrix
a$set(matrix(c(-2,0,0,0,-2,0,0,0,-2),3,3))
a$get()         # get inputed matrix
a$getInverse()  # return null,because the inverse matrix haven't be calculated

cacheSolve(a)  # calculate inverse matrix 
cacheSolve(a)  # get invert matrix in cache

a$get()        # get inputed matrix ,proved that it haven't be changed
a$getInverse()   # get invert matrix in cache


