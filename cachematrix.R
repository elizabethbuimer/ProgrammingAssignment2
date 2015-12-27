## Returns the inverse of matrix x 

## makeCacheMatrix calculates the inverse of matrix x and saves it in the cache memory

makeCacheMatrix <- function(x = matrix()) {	# Call function as follows: L<-makeCacheMatrix(x) Where x is a square invertible matrix
	inverse <- NULL
	set <- function(y) {
		x <<- y 					# Substitute old x with new input matrix 
		inverse <<- NULL 				# Resetting inverse to NULL 
	}
	get <- function() x 
	setsolve <- function(solve) inverse <<- solve	 	# Store input variable and function
	getsolve <- function() inverse				# Return it	
	list(set=set, get=get, 
	     setsolve=setsolve,
	     getsolve=getsolve)
}


## cacheSolve inverses matrix x either by calculating it or by reading it from the cache memory

cacheSolve <- function(L) {						# L is the output from the makeCacheMatrix function
      ## Return a matrix that is the inverse of original input matrix 'x'
	inverse <- L$getsolve()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- L$get()
	inverse<- solve(data)
	L$setsolve(inverse)
	inverse
}