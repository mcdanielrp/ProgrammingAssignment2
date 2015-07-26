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
        setmean <- function (mean) m <<- mean
        getmean <- function () m
        list(set = set, 
                get = get,
                setmean = setmean,
                getmean = getmean)

}


## This function extracts cached matrix defined in this environment

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmean()
        if(!is.null(m)){
        
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- mean(data,...)
        x$setmean(m)
}

