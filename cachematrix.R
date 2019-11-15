## Put comments here that give an overall description of what your
## functions do
## These two functions will set and cache the matrix. Then get the inversed cache matrix.

## Write a short comment describing this function
## makeCacheMatrix is composed by a list of 4 functions including: 
## 1. set the matrix 
## 2. get the matrix, 
## 3. set the inversed matrix 
## 4. get inversed matrix.
## makeCacheMatrix will check weather the input matrix can be inversed firstly.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		if (nrow(y) == ncol(y)) {
		    x <<- y
		    m <<- NULL
		} else {
		    message("the shape of matrix has to be square, so it can be inversed ")
		}
	}
	get <- function() x
	setinversedmatrix <- function(inversed_matrix) m <<- inversed_matrix
	getinversedmatrix <- function() m
	list(set = set, get = get, setinversedmatrix = setinversedmatrix, getinversedmatrix = getinversedmatrix)
}


## Write a short comment describing this function
## cacheSolve will use solve func calculate the inversed matrix of the matrix has been cached.
cacheSolve <- function(x, ...) {
        # if the inversed matrix has already been calculated then get the cached data
        m <- x$getinversedmatrix()
        if(!is.null(m)) {
        	message("getting the inversed matrix")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversedmatrix(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
