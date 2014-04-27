## The following two functions are used to create a special object to store a matrix and cache's its inverse.


## The makeCacheMatrix() will create a special matrix object that will cache the inverse matrix.

makeCacheMatrix <- function(matrix = matrix()) {
	#invMatrix - inverse matrix value
	invMatrix <- NULL #default value - NULL

	#setMatrix function: matrix and reset invMatrix
	setMatrix <- function(y){
		matrix <<- y
		invMatrix <<- NULL
	}
	#getMatrix function: get matrix
	getMatrix <- function() matrix

	#setInvMatrix function: set inverse matrix
	setInvMatrix <- function(inverseMatrix) invMatrix <<- inverseMatrix

	#getInvMatrix function: get inverse matrix
	getInvMatrix <- function() invMatrix

	list(setMatrix = setMatrix, getMatrix=getMatrix,
		 setInvMatrix = setInvMatrix,
		 getInvMatrix = getInvMatrix)
}



## The cacheSolve() will conpute the inverse of the special matrix returned by makeCacheMatrix(). If the inverse has 
## been calculated and the matrix has not changed, then the cacheSolve() will return the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #get invMatrix
        invMatrix <- matrix$getInvMatrix()

        #check if it is not null
        if (!is.null(invMatrix)){
        	message("getting cached data")
        	return (invMatrix)
        }

        #if it is null, calculate the inverse
        data <- matrix$getMatrix()
        inv <- solve(data, ...)
        matrix$setInvMatrix(inv) #save it
        inv
}
