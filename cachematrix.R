## author: r mcdaniel
## date: 7-26-15
##


## The function below sets a matrix 'x' to the cache
## so it can then be accessed quickly later.


makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y){
        
                x <<- y
                m <<- NULL
        
        }
        
        ## setting up get,setmean,getmean
        
        get <- function () x
        setsolve <- function (solve) m <<- solve
        getsolve <- function () m
        list(set = set, 
                get = get,
                setsolve = setsolve,
                getsolve = getsolve)

}


## This function extracts cached matrix defined in this environment

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
        if(!is.null(m)){
        
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data,...)
        x$setsolve(m)
        m
}

