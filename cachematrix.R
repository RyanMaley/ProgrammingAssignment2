## Course:      R Programming (Coursera rprog-32)
## Assignment:  cacheMatrix - Programming Assignment 1 - Week 3
## Author:      Ryan Maley
##

makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a specical "matrix" object that can cache its inverse.
        
        ## Prep to set or get the matrix  
        S <- NULL
        set <- function(y) {
                x <<- y
                S <<- NULL
        }
        get <- function() x
        
        ## Define the inverse matrices solution- Set it or Get it
        setsolve <- function(solve) S <<- solve
        getsolve <- function() S

        ## Define the list which is returned by the function
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}

cacheSolve <- function(x, ...) {
        ## This function computes the inverse of the "special"matrix retrned by
        ## cacheMakeMatrix above. If the inverse has already been calculated 
        ## (and the matrix has not changed), then cacheSolve should retreive 
        ## the inverse from the cache.

        S <- x$getsolve()       ## Something in the cache?

        if(!is.null(S)) {       ## The cache has something in it, so just return it
                message("Getting cached data")
        } else {                ## Cache is empty so calculate
                message("Calculating")  
                data <- x$get()
                S <- solve(data, ...)   ## Actual inverse calculation & assignment
                x$setsolve(S)
        }
        return(S)
}

TestSolution <- function() {
        ## This function tests the above functions with sample data. I should have
        ## a test somewhere for an invertible matrix but you shoul djust be able
        ## to set TestMatrix to something invertible, source it, then 
        ## run TestSolution()
        
        ## Create some sample Data 
        ## TestMatrix <- rbind(c(1, 2), c(3,4))         ## Very simple 2x2 matrix
        
        r = rnorm(1000000)                              ## Something bigger to show the time difference
        TestMatrix = matrix(r, nrow=1000, ncol=1000)

        ## Prime the list
        TestList<- makeCacheMatrix(TestMatrix)

        ## Run twice to show the difference in calculations vs return from cache
        message('First Pass')
        cat("Matrix:",TestMatrix[1:10],"\n")
        StartTime <- Sys.time()
        TestInverse <- cacheSolve(TestList)
        print(Sys.time() - StartTime)
        cat("Inverse:",TestInverse[1:10],"\n")
        
        message('Second Pass')
        cat("Matrix:",TestMatrix[1:10],"\n")
        StartTime <- Sys.time()
        TestInverse <- cacheSolve(TestList)
        print(Sys.time() - StartTime)
        cat("Inverse:",TestInverse[1:10],"\n")
}
