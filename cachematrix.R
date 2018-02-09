## Autor : David 
## Date: 09-02-2018
## W10-x64
## AW R15, ram16,
"
******** Assignement 2

"

"
This function initialise the 'mat' matrix in order to create a vector with 4 parts. 
"
makeCacheMatrix <- function(mat = matrix()) {  # Specify that our input is a matrix
  inv <- NULL                                  # Initialise mat as NULL in the current environement
  set <- function(y=matrix()){                 # Function to set variable name to modify the parent function
    mat<<- y
    inv<<- NULL
  }
  
  get<- function() mat                         # Specify the matrix to be used
  set_inv <- function(inv) inv <<- solve       # Solve (inverse) the matrix
  get_inv <- function() inv                    # Set the ghost variable 
  
  list(set=set,get=get,set_inv=set_inv,get_inv=get_inv) # Create a list of setting to be used by cacheSolve function

}


## Write a short comment describing this function
"
This function will take the initialized data from makeCacheMatrix function and look for specific **names** and will check if
the data is already available in the cached part of the memory. if it is not il will be computed again
"
cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- mat$get_inv()
  
  if(!is.null(inv)){
    print("we already have the data")
    return(inv)
  }
  
  else {
    our_mat <- mat$get()
    inv <- solve(our_mat,...)
    mat$set_inv(inv)
    inv
  }
  
}
