## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function is creates to cache the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # initializing inv to NULL
        set <- function(y){
                # refresh x and inv in case i later set
                x <<- y 
                inv <<- NULL
        }
        get <- function() x # put the x outside of get function, so R retrieves it from the parent environment
        setinverse <- function(inverse) inv <<- inverse # put inverse into parent environment inv
        getinverse <- function() inv # put the inv outside of getinverse function, so R retrieves it from the parent environment (NULL)
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse) # naming these functions in order to use "$" later
}


## Write a short comment describing this function.

# This fuction is used to calculate the value of inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse() #assign getmean to inv，should be NULL when i first run
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get() #assign the initial matrix to data
        inv <- solve(data, ...) 
        x$setinverse(inv) #cache inv
        inv
}
