## Put comments here that give an overall description of what your
## functions do

## The first function caches the matrix to which the function gehts applied. Furthermore it defines the functions which are later used in the cache Solve function

## The second function checks if the inverse of the matrix has already been calculated. If so the chached inverse is returned. Otherwise the inverse is calculated from the cached matrix and then returned

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL 			# m is a free variable, so the matrix x gets set to 0
	set <- function(y){	# y is a free variable 
		x <<- y			# assignes the value of y to the matrix
		m <<- NULL
	}
	get <- function()x	# function x gets assignet to get
	setinv <- function(solve)m <<- solve		
	getinv <- function()m
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	m <- x$getinv()				# the inverse of the function is stored in m 
	if(!is.null(m)){			# checks if the inverse has been calculated
		message("getting cached data")	# if the inverse has been calculated this message is printed and ...
		return(m)				# and m where the inverse was previosly stored is returned
	}
	data <- x$get()			# if the inverse has not been calculated the cached matrix is stored in m
	m <- solve(data, ...)	# the solve function is applied to the matrix stored in m
	x$setinv(m)
	m
        ## Return a matrix that is the inverse of 'x'
}
