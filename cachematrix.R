## The 1st function sets and outputs cache matrix created by users.
## The 2nd function calculates the inverse of the initial matrix provided by the 1st function.

## Please see the detailed comments inside the 1st function 

makeCacheMatrix <- function(x = matrix()) {
  
  # Create the initial varlabie 'inv' as NULL
  inv <- NULL
  
  # The object that helps users create an initial matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # This object outputs the initial matrix on the console after it is set
  get <- function() x
  
  # This object makes up an inverse matrix
  set_Inv_Matrix <- function(inverse) inv <<- inverse
  
  # This object stores the inverse matrix calculated by cacheSolve()
  get_Inv_Matrix <- function() inv
  list(set = set, get = get,
       set_Inv_Matrix = set_Inv_Matrix,
       get_Inv_Matrix = get_Inv_Matrix)
}


## Please see the detailed comments inside the 2nd function

cacheSolve <- function(x, ...) {
  
  # This line return a inverse matrix due to the given matrix 'x' 
  inv <- x$get_Inv_Matrix()
  
  # Return this message if 'inv' has been set
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Assign the matrix to the variable 'data'
  data <- x$get()

  # This line calculates the inverse of matrix by the bulit-in function solve()
  inv <- solve(data)

  # Set the inverse to the variable 'x'
  x$set_Inv_Matrix(inv)

  # Return the calculated inverse matrix as 'inv'
  inv
}
