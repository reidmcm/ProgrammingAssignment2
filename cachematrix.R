#A pair of functions, "cacheIV" and "makeCacheableMatrix"  which, respectively, cache the inverse of a matrix if it has 
#not been stored already, and return a list of 4 basic functions used to cache the matrix's inverse.  Modelled after 
#example code from JHU's R programming coursera course, assignment 2. 


# this function takes as input a matrix and returns a list of functions required to make a cached inverse of it.  
#This list can then be input as an argument of cacheSolve to cache the matrix's inverse.  See below for descriptions of each of the 4 functions

makeCacheMatrix <- function(matrix1 = matrix()) {
	
#set IV to null within this environment
	IV = NULL
	
	
#sets matrix the matrix value in the parent environment, (re)sets the IV value to NULL in the parent environment
	setMatrix = function(y){
		matrix1 <<- y
		IV <<- NULL
	}
	
#define getMatrix, a function that merely returns the matrix matrix1
	getMatrix = function() matrix1
	
#define setIV, a function that sets IV in the parent environment equal to the value input for the argument "inverse" 
	setIV = function(inverse) IV <<- inverse	
	
#define getIV, a function that merely returns the inverted matrix.  why not just input that value directly to the list?
	getIV = function() IV
	
#define fnList as a list comprised of the functions previously defined within this function, return fnList	
	fnList = list(setMatrix = setMatrix, getMatrix = getMatrix, setIV = setIV, getIV = getIV)	

}


#this function takes as input the output list of functions from makeCacheMatrix, checks to see if the inverse of the corresponding
#matrix has been calculated and, if so, returns it.  if not, calculates it then returns it

cacheSolve <- function(x, ...) {
	IV = x$getIV()
	
#checks if the getIV() function has returned a non - NULL value, in which case the fn returns the cached value and calls it a day
	if (!is.null(IV)){
		message ("retrieving cached matrix inverse")
		return(IV)
	}
	
#sets the variable matrix = to the value returned by function get in list fnList
	matrix = x$getMatrix()
	
#solves the matrix for its inverse (assuming it's square and invertible)
	IV = solve(matrix, ...)

#call the function setIV from within fnList to cache the calculated IV value as the IV variable in the parent environment
	x$setIV(IV)
#return the inverse matrix
	IV
}
