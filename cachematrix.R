## Matrix inversion functions 
## R programming assignment 2

## set and get matrix inversion and listing functions

makeCacheMatrix <- function(x = matrix()) {
		iv <- NULL						##inverse to null
		set<-function(mt){				##Set up matrix
			x<<-mt
			iv<<-NULL
		}
		get<-function()x					##get matrix

		set.inv<-function(setinv) iv<<-setinv	##set up inverse
		get.inv<-function()iv				##get inverse
		list(set=set, get=get,				##Listing of set and get funtions
			set.inv=set.inv,
			get.inv=get.inv)
}


## Matrix inversion by cache and store, 
##if solved returns cache unless otherwise do calculate

cacheSolve <- function(x, ...) {
        								## Return a matrix that is the inverse of 'x'
	iv<-x$get.inv()						##get stored inverse value

	if(!is.null(iv)){						##if function to return value if stored
		message("getting stored data")
		return(iv)
	}
	mat<-x$get()						##else not stored calculate and store value
	iv<-calc(mat,...)
	x$set.inv(iv)
	
	iv								##inverse value returns
}

