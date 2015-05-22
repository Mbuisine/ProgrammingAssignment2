## makeCaheMatrix is designed to calculate and store the inverse of a matrix. cacheSolve checks first in makeCaheMatrix that the inverse doe not already exist. If not it calculates the inverse and stores its value in makeCacheMatrix. 


## makeCacheMatrix is  a fuction that stores M, the inverse of a matrix. If the inverse has not been calulated then the matrix is NULL.


## step 2: we define M as a null matrix. It is equal to null if we don't have the inverse matrix stored
## step 3: the function set changes the matrix stored in the main function


makeCacheMatrix <- function(x = matrix()) {

## the matrix is NULL at the begining

	M <- NULL

## We create the function set to store the value of the matrix. As it's a new matrix the matrix M comes back to NULL as 

  	set <- function(y) {
    		x <<- y
   		M <<- NULL
	}

## we create functions to get and set the inverse of the function 

 	get <- function() x
 	setsolve <- function(solve) M <<- solve
 	getsolve <- function() M
  	list(set = set, get = get,  setsolve = setsolve, getsolve = getsolve)
}




## cacheSolve calculates the inverse of a matrix x


cacheSolve <- function(x, ...) {

## checks is the matrix M already exists in the other function. If it does, just read its value and display it

  	M <- x$getsolve()
 	 if(!is.null(M)) {
   	 	message("getting cached data")
 	  	return(M)
 	 }

## if the inverse does not existe, put the matrix in "data" and calculate its inverse that we call M. Save M in the other function and display M

	 data <- x$get()
	 M <- solve(data, ...)
	 x$setsolve(M)
 	 M
}


