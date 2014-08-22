# The makeCacheMatrix() function has sub functions whose description id provided below:
#setmatrix() - sets a new matrix from the value
#getmatrix - returns the matrix that has been set
#cacheInv - stores the inverted matrix in cache
#getInv - returns the cached value

# The cacheSolve() function checks if the value is present in cache. If present, it returns the value. Else, calculates the inverse. Stores the value in cache and returns the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {

	#initializing the cache value
	cache <- NULL
	
	#setting a new matrix. Since the matrix is being assigned for the first time, the value of cache is set to NULL
	setmatrix <- function(value){
		x <<- value
		cache <<- NULL
	}
	
	#This functions returns the matrix set
	getmatrix <- function(){
		x
	}
	
	#stores the given matrix onto the cache
	cacheInv <- function(solve){
		cache <<- solve
	}
	
	#returns the cache 
	getInv <- function(){
		cache
	}

	list(setmatrix = setmatrix, getmatrix = getmatrix, cacheInv = cacheInv, getInv = getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	
	#checks if the value is in cache (if present, it prints out "getting cached data..." and returns the cached data) or else gets new data.
	inverse <- x$getInv()
	if(!is.null(inverse)) {
        message("getting cached data...")
        return(inverse)
    }
	
	#get the matrix of x 
	data <- x$getmatrix()
	
	#get the inverse of the matrix
	inverse <- solve(data)
	
	#cache the solved data
	x$cacheInv(inverse)
	
	
    ## Return a matrix that is the inverse of 'x'
	inverse	
		
		
}
