## The function "makeCacheMatrix" creates a matrix and a cached inverse of that matrix

## The function "cacheSolve" creates the inverse of a matrix using the cached value
## If the cached value is not available it computes it and caches it - creating an inverse of the orginal matrix


## "makeCacheMatrix" function creates 4 other functions ("set_M", "get_M", "setInverse_M", and "getInverse_M")
## "x" argument sets the orginal matrix 
## See comments by lines for further details

makeCacheMatrix <- function(x = matrix()) {

  cache <- list() ## This list will store the cached inverse
  set_M <- function(z) { ## Sets up the matrix data 
    x <<- z ## Takes argument "z" and assigns "z" to "x" in the environment of the "makeCacheMatrix" function 
    cache$inverse_M <- NULL 
  }
  get_M <- function() { ## Get the matrix which is stored in "x"
    x
  }
  setInverse_M <- function(inverse){ ## Set the cached inverse of the matrix from above
    cache$inverse_M <<- inverse ## Store that cached matrix here 
  }
  getInverse_M <- function() { ## Get the cached inverse of the matrix stored above
    cache$inverse_M ## Cached inverse of matrix is stored in here so return this
  }
  list(set_M = set_M, get_M = get_M, setInverse_M = setInverse_M, getInverse_M = getInverse_M) ## Interface to set and get the matrix and its inverse
}



## This function creates the inverse of a matrix using the cached value or creates it and caches it 
## See comments by lines for further details

cacheSolve <- function(x, ...) {
  c_inverse_M <- x$getInverse_M() ## Get the cached inverse from "x" argument in "makeCacheMatrix" function
  if(!is.null(c_inverse_M)) {
    message("Cached inverse available") ## If the cached inverse was calculated from the "makeCacheMatrix" function this function returns "Cached inverse available"
    return(c_inverse_M) ## Return the cached inverse matrix
  } else { ## If the cached inverse of the matrix was not calculated from the "makeCacheMatrix" function
    inverse <- solve(x$get_M(), ...) ## Get the orginal matrix 
    x$setInverse_M(inverse) ## Inverse that original matrix 
    return(inverse) ## Return the computed inversed matrix 
  }
      
}

