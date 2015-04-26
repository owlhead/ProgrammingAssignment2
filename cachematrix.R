## The following function caches the inverse of a matrix

## The following codes have been used to generate data for a 
## 7 by 7 square matrix, hence the size 49.
# set.seed(1)
y <-sample(1:30, size= 49, replace=TRUE) 


## The following function creates the matrix and then caches its inverse
##  - The function setm takes a new matrix
##  - getm retrieves the provided matrix
##  - setinv function sets a value (the inverse matrix) for the variable inv
##  - getinv retrieves the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setm <- function(x2){
    x <<- x2
    inv <<- NULL
  }
  getm <- function(){x}
  setinv <- function(inverse){inv <<- inverse}
  getinv <- function(){inv}
  list(set=setm, get=getm, setinv= setinv, getinv = getinv)
}


## The following function computes the inverse of the matrix if
## it has not been already determined, i.e. if the matrix has been
## changed. Otherwise it returns the old inverse matrix
cacheSolve <- function(x, ...) {
        inv <- x1$getinv()
        if(!is.null(inv)){
          message("retrieving cached data")
          return(inv)
        }
        mdata <- x1$get()
        inv <- solve(mdata,...)
        x1$setinv(inv)
        inv
}

## Test run:
## The makeCacheMatrix has been assigned to the vairable x1
x <- matrix(y, 7,7)
x1 <- makeCacheMatrix(x)
x1$getinv() # retrieving the inverse matrix
cacheSolve(x)
