## Put comments here that give an overall description of what your
## functions do
## 
## Write a short comment describing this function
## ## makeCacheMatrix is composed by a list of 4 functions including: set the matrix, get the matrix, set the inversed matrix and get
## inversed matrix.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinversedmatrix <- function(inversed_matrix) m <<- inversed_matrix
	getinversedmatrix <- function() m
	list(set = set, get = get, setinversedmatrix = setinversedmatrix, getinversedmatrix = getinversedmatrix)
}


## Write a short comment describing this function
## cacheSolve will use calculate the inversed matrix of the original matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(m)) {
        	message("getting the inversed matrix")
        }
        data <- x$get()
        m <- solve(data)
        x$setinversedmatrix(m)
        m
}
