## Put comments here that give an overall description of what your
## functions do

#### Write a short comment describing this function:
## This function is getting a matrix input, using lexical scoping to determine if the data input is null or not, and creating a list of 4 functions (getting the matrix, setting the matrix, 
##getting the inverse, setting the inverse) that can more easily be used when called upon. 
##This also stores the output (ie the inversed matrix) in a cache which creates a shortcut for the next function if the parameters have been previously calculated so you only need to claculate 
##it once and pull the value continually. 

makeCacheMatrix <- function(LargeMatrix = matrix()) {
        calculated_inverse <- NULL
  set_matrix <- function(new_matrix) {
    LargeMatrix <<- new_matrix
    calculated_inverse <<- NULL
  }
  get_matrix <- function() LargeMatrix
  set_inverse <- function(solve) calculated_inverse <<- solve
  get_inverse <- function() calcuated_inverse
  list(set_matrix = set_matrix,
       get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}


#### Write a short comment describing this function:
## This function takes the same inputs as the makeCacheMatrix function to solve for the matrix's inverse. 
## If the cache created in the previous function doesn't apply, it will calcuate the inverse itself.
## If there is a relevant cached value from the previous function, it will display "getting cached data" and return the matrix's inversed value (thus saving lots of computing time).

cacheSolve <- function(LargeMatrix, ...) {
        calculated_inverse <- LargeMatrix$get_inverse()
  if(!is.null(calculated_inverse)) {
    message("getting cached data")
    return(calculated_inverse)
  }
  data <- LargeMatrix$get_matrix()
  calculated_inverse <- solve(data, ...)
  LargeMatrix$set_inverse(calculated_inverse)
  calculated_inverse
        ## Return a matrix that is the inverse of 'x'
}
