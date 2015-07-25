## Accepts a matrix from the user and calculates its inverse only if it has not already been calculated.
## It caches the value of the inverse matrix and stores it for reference.

## The first function will accept a square matrix and create a list containing a function to set the value,
## ...of the matrix, get the value, set the value of its inverse, and get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL

        set <- function(y) {

                x <<- y

                inv <<- NULL

        }

        get <- function() {x}

        setinv <- function(solve) {inv <<- solve}

        getinv <- function() inv

        list(set = set, get = get,

             setinv = setinv,

             getinv = getinv)

}


## Checks if the value of matrix is already calculated. If its calculated, returns the cached data.
## If it is not calculated, it calculated it using the solve() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		  inv <- x$getinv()

        if(!is.null(inv)) {

                message("getting cached data")

                return(inv)

        }

        data <- x$get()

        inv <- solve(data, ...)

        x$setinv(inv)

        inv

}
