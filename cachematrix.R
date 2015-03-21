# Matrix inversion is a process which can be costly for the computer, specially when the matrix is too large.
# In these situations, caching the inverse of a matrix rather than calculate it repeatedly can be beneficial
# because the user may save some time and get the final result faster.
# The following two functions are used to cache the inverse of a matrix and return its value.

# makeCacheMatrix creates a list containing a function to:
# 1 - set the value of a given matrix, in this case, denominated x
# 2 - get the value of the same matrix
# 3 - set the value of inverse of the matrix x
# 4 - get the value of inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL                                                         
                                        
  set <- function(y) {                  
                                        
    x <<- y                             
    m <<- NULL                          
                                        
  }
  get <- function() x                                                     
                                        
  setsolve <- function(solve) m <<- solve 
                                      
  getsolve <- function() m                                           
                                        
  list(set = set, get = get,            
       setsolve = setsolve,               
       getsolve = getsolve)
}


# The function bellow assumes that the matrix given previously is always invertible.
# This function first checks if the inverse has already been calculated. If it has been, it skips the calculation
# and simply returns the value of the inverse. Otherwise, it calculates the inverse and sets the value in the cache.


cacheSolve <- function(x, ...) {
  m <- x$getsolve()                       
                                        
  if(!is.null(m)) {                                                
                                        
    message("getting cached data")
    return(m)
  }
  data <- x$get()                       
                                                           
  m <- solve(data, ...)                  
                                        
  x$setsolve(m)                          
                                       
  m                                     
}