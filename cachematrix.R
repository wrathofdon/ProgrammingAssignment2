## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Function creates a list of methods, which contains matrix valus and inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        #  This signifies that the inverse has yet to be calulcated
        set <- function(y) {
                x <<- y
                inv <<- NULL
                # When a new matrix is set, we must re-calculate the inverse
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        # setinverse allows us to input the new inverse value
        getinverse <- function() inv
        # getinverse says whether or not the inverse has been calculated
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                # if inverse has already been solved, then we retrieve it
                message("getting cached data")
                return(inv)
        }
        # if the inverse has not been solved, then we do that now.
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv)
        # once the inverse has been solved, we no longer have to solve it again.
        inv
        # finally, we return the inverse.
}
