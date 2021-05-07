#The first function, `makeCacheMatrix` creates a special "matrix", which is
#really a list containing a function to

#1.  set matrix
#2.  get matrix
#3.  set inverse of the matrix
#4.  get inverse of the matrix

#The second function calculates the inverse of the special "matrix"
#created with the first function. However, it first checks to see if the
#inverse has already been calculated. If so, it `get`s the inverse from the
#cache and skips the computation. Otherwise, it calculates the inverse of
#the data and sets the inverse of the matrix in the cache via the `setInverse`
#function.


## This function creates a special "matrix" object
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(matrix){   #set a matrix
        x <<- matrix
        i <<- NULL
    }
    get <- function() x #get the matrix
    setinverse <-function(inverse) i <<- inverse #set inverse of the matrix
    getinverse <- function() i #get inverse of the matrix
    list(set = set, get = get,  #return the list
         setinverse = setinverse,
         getinverse = getinverse)
}

##This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){ #return existed cached inverse of the matrix
        message("getting cached data")
        return(i)
    }
    data <- x$get() #get the matrix from the object
    i <- solve(data,...) #calculate the inverse of the matrix
    x$setinverse(i) #set the inverse of the object
    i #return the inverse of the matrix
}
