# makeCacheMatrix and cachSolve are a pair of functions to cach the inverse of a matrix.
# makeCacheMatrix creates a object to cach the matrix and its inverse, cachSolve calculates the inverse using cached values from such object.



# makeCacheMatrix creates a special "matrix" object that stores a matrix and can cach the inverse of that matrix
# makeCacheMatrix returns a list of functions:
# setMatrix to store a matrix
# getMatrix to retrieve the matrix
# setinverseMatrix to store the inverse of the matrix
# getinverseMatrix to get the inverse of the matrix
makeCacheMatrix <- function(Matrix = matrix())
{
      inverseMatrix <- NULL
      setMatrix <- function(y)
      {
            Matrix <<- y
            inverseMatrix <<- NULL
      }
      getMatrix <- function() Matrix
      setinverseMatrix <- function(invMatrix) inverseMatrix <<- invMatrix
      getinverseMatrix <- function() inverseMatrix
      list(setMatrix = setMatrix,
           getMatrix = getMatrix,
           setinverseMatrix = setinverseMatrix,
           getinverseMatrix = getinverseMatrix
      )
}


# cachSolve calculates the inverse of a matrix, where the argument x is a special matrix created with function makeCacheMatrix for the matrix.
# cachSolve first checks whether the inverse has already been calculated. In that case the cached inverse matrix is returned.
# Otherwise the inverse of the matrix is calculated and cached via the setinverseMatrix function of x.Then the inverse of the matrix is returned.
# A precondition is that the matrix is invertible.
cacheSolve <- function(x, ...)
{
      inverseMatrix <- x$getinverseMatrix()
      if(!is.null(inverseMatrix))
      {
            message("getting cached data")
            return(inverseMatrix)
      }
      matrix <- x$getMatrix()
      inverseMatrix <- solve(matrix, ...)
      x$setinverseMatrix(inverseMatrix)
      inverseMatrix
}