# The two functions below create a special object
#that stores a numeric matrix and caches its inverse. 

## This function (makeCacheMatrix) creates a list containing a function to
##set the value of the matrix (=set)
##get the value of the matrix (=get)
##set the value of the inverse (=setsolve)
##get the value of the inverse (=getsolve)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function (cacheSolve) looks for the inverse of the matrix in the cache. 
##If it can find it it returns it (getsolve).Otherwise, it will caculate the inverse of the matrix(solve),
##return that new matrix, as well as set it in the cache (setsolve).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        if(!is.null(i)) {
          message("getting cached inverted matrix")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}

