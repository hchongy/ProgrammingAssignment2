## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse_matrix <- function(inverse_matrix) m <<- inverse_matrix
        get_inverse_matrix <- function() m
        list(set = set, get = get,
             set_inverse_matrix = set_inverse_matrix,
             get_inverse_matrix = get_inverse_matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$get_inverse_matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse_matrix(m)
        m
}
