## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
### This function, makeCacheMatrix, creates a a list containing a function to
### 1. set the matrix
### 2. get the matrix
### 3. set the inverse of the matrix
### 4. get the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL # initializes the inverse to NULL
        
        # This function sets the matrix m to y and initializes the inverse to NULL
        set <- function(y) {
                m <<- y
                inv <<- NULL
        }
        
        # This functions return the matrix m
        get <- function() { 
                m
        }
        
        # This function set the inverse of the matrix
        setinverse <- function(inverse) {
                inv <<- inverse
        } 
        
        # This function returns the inverse of the matrix
        getinverse <- function() {
                inv
        }
        
        # This creates the list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
### This function, cacheSolve, calculates the inverse of the matrix created with the above function. 
### However, it checks first to see if the inverse has already been computed. If so, it gets the 
### inverse from the cache and skips the computation. Otherwise, it computes the inverse of the matrix
### and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
        inv <- m$getinverse()
        
        # checks if the inverse has already been computed
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # if not, retrieves the matrix and computes the inverse using the solve function
        data <- m$get()
        inv <- solve(data, ...)
        m$setinverse(inv)
        inv
}
