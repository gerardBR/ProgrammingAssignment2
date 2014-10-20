# This function allows us to Matrix object and cache Inverse of it.
makeCacheMatrix <- function(x = matrix()) {
        ##We start with the inverse property
        i <- NULL
        ##Then we set the Matrix 
        set <- function(matrix){
                x <<- matrix
                i <<- NULL
        }
        ##Method to get the matrix
        get <- function() {x}
        
        ##Same thing that in the last to lines of code, but for the Inverse Matrix
        setInverse <- function(inverse){
                i <<- inverse
        }
        getInverse <- function(inverse){i}
        ##this command makes a list of the methods used to set/get the matrix
        ##and the inverse of it.
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## this function below, calculates the inverse of the Matrix, but first,
##it checks if it has already been calculated, if it is stored, then instead of
##making the computation, shows the past results.

cacheSolve <- function(x, ...) {
        ##get the matrix from the previous function if the Inverse has been calculated.
        ##If that is true, return it
        i <- x$getInverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        ##If the Inverse of the matrix has not been calculated, the do it and print it.
        ##It stores the result if we need it in the future.
                data <- x$get()
        ##after getting the matrix from the object, we solve it by using the matrixs operator.
                i <- solve(data) %*% data
        ##set the inverse of the matrix and store it.
                x$setInverse(i)
        ##Print the inverse
                i
       
}
