## makeCacheMatrix creates a special matrix that can cache its inverse

## This function has one argument of matrix type.
## It initializes matrix_inverse as NULL
## then it defines a set function requiring one argument
## it assigns x in the parent environment of set function to another_matrix
## and matrix_inverse as NULL in the set function environment
## get function return matrix x
##setinverse sets the inverse of the matrix
##getinverse returns the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse<-NULL
  set<-function(another_matrix){
    x<<-another_matrix
    matrix_inverse<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) matrix_inverse<<- inverse
  getinverse<-function()matrix_inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above.If the inverse is already calculated 
## and the matrix has not changed then cacheSolve retrieves the 
##inverse from the cache

## the first statement assigns the matrix_inverse 
## as the already calculated inverse
##if it is non null, it returns the cached value of the inverse
## If x matrix has changed it recalculates inverse using solve function 
## and it returns the updated value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_inverse<-x$getinverse()
  if(!is.null(matrix_inverse)){
    message("get the cached inverse")
    return(matrix_inverse)
  }
  matrix_data<-x$get()
  matrix_inverse<-solve(matrix_data,...)
  x$setinverse(matrix_inverse)
  matrix_inverse
}
