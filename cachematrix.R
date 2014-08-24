## Functions "cacheSolve" and "makeCacheMatrix" work together to return
## the inverse of a given matrix by first checking it had previously been
## calculated, in which case they just return the stored value 



## makeCacheMatrix is a function returning a list of four functions used by
##  cacheSolve to access the properties of a matrix and its inverse

makeCacheMatrix  <- function( x = matrix()) {

	# every time you call this makeMatrix function
	# with new matrix data
	# it will create a new matrix object, with 4 methods
	# and it will erase any previous inverted matrices  
	# stored in memory 

	storedInvMatrix <- NULL

	# use the following functions (methods in oo terminology)  to 
	# read or manipulate a previously created matrix (object in oo terminology)


	# "get" simply returns the matrix you want to invert
	get <- function() x


	# "set" simply changes the matrix data
	# of a previously created matrix object
	# it will reuse/overwrite an already existing matrix object
	# and make sure that any stored inverted matrix is set to NULL
	set <- function(new_matrix) {
	   storedInvMatrix <<- NULL
	   x <<- new_matrix	
	}


	# "storeCalcInvMatrix" takes the calculated inverted matrix 
	# and stores the result to a matrix variable: storedInvMatrix

	storeCalcInvMatrix <- function(result)  storedInvMatrix <<- result


	# "getInvMatrix" returns a previously stored inverted matrix
 	getInvMatrix <- function() storedInvMatrix

	list(set = set, get = get, storeCalcInvMatrix=storeCalcInvMatrix, getInvMatrix=getInvMatrix)

} 



## cacheSolve takes a matrix x and returns its inverse
## it first checks if it already calculated and stored in memory
## if it is not there, it will calculate and store it

cacheSolve  <- function(f, ...) {

	# see if you have already calculated 
	# and stored in memory the inverted matrix
        cachedInvMatrix <- f$getInvMatrix()

	# if you get something, print it 
	# informing that this comes from memory
	# and stop execution
        if(!is.null(cachedInvMatrix)) {
                message("getting cached inverted matrix")
                return(cachedInvMatrix)
        }


	# if nothing came from memory,
	# get the matrix 
        matrix_data <- f$get()

	# calculate the inverted matrix
        invMatrixData <- solve(matrix_data, ...)

	#store the calculated inverted matrix in memory
        f$storeCalcInvMatrix(invMatrixData)

	#and, finally, print the calculated inverted matrix
        invMatrixData
}
