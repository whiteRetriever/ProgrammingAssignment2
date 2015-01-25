## Matrix inversion 
## following functions is used to cache the inverse of a matrix
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setInverseMatrix <- function(inverseMatrix) inverse <<- inverseMatrix
      getInverseMatrix <- function() inverse
      list(set=set, get=get, setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)
}



## The following function returns the inverse matrix of the given matrix. 
## Function in first step checks if the inverse matrix has already been computed.
## If it is TRUE, it gets the result and skips the computation. 
## In second step, function also checks if whether there is any changes 
## in a given matrix. If not, it computes the inverse, sets the value in the 
## cache via setInverseMatrix function.
## The assumption of this function is that given matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getInverseMatrix()
      a<- x
      b<-m$get()
      if(!is.null(inverse)) {
            if(identical(a, b)){    ##Compare two matrices (TRUE or FALSE)
                  message("getting cached data.")
                  return(inverse)
            }
      }
      data <- x$get()
      inverse <- solve(data)
      x$setInverseMatrix(inverse)
      inverse
}
}
