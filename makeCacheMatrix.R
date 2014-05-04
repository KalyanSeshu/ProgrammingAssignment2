#This assignment introduces the <<- operator which can be used to 
#assign a value to an object in an environment that is different from
#the current environment.

#Below are two functions that are used to create a special object that
#stores a [numeric] matrix and its dimension and cache's its inverse.

#The first function, makeCacheMatrix creates a special "matrix", which is
#really a list containing a function to:

#1.    set the value of the matrix
#2.    get the value of the matrix
#3.    set the inverse of the matrix using solve()
#4.    get the inverse of the matrix using solve()

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

#The second function calculates the inverse of the special "matrix"
#created with the above function. However, it first checks to see
#if the inverse has already been calculated. If so, it gets the inverse
#from the cache and skips the computation. Otherwise, it calculates
#the inverse of the data and sets the value of the inverse in the cache via
#the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}