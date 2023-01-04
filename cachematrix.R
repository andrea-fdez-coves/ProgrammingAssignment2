## Programming Assignment 2: Lexical Scoping
## Week 3 of the R Programming course from Coursera (r)

## Original code:rdpeng (Roger D. Peng, Professor of Statistics and Data 
## Sciences at the University of Texas, Austin) 
## Edited by: Andrea Fernández Coves on Jan 2023


## In this assignment, we need write a pair of functions that catche the 
## inverse of a matrix

#makeCacheMatrix creates a special "matrix" object that can cahche its inverse
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        #we will use the solve function to calculate the inverse
        set <- function(y){
                x <<-y
                s <<- NULL
        }
        get <- function() x
        setInv <- function(inv) s<<-inv
        getInv <- function() s
        list(set=set, get=get, setInv=setInv, getInv=getInv)

}


#cacheSolve computes the inverse of the special matrix returned by the function
#"makeCacheMatrix" (above). If the inverse was already calculated (i.e., the
# matrix has not changed), then this function should retrieve the inverse from
# the cache

cacheSolve <- function(x, ...) {
        # The first thing will do is to check if the matrix has been inversed,
        #if so, we will get the inverse (Result fros solve) from the cache and
        # skip the computation
        s <- x$getInv()
        if (!is.null(s)){
                message ("getting chaged data")
                return(s)
        }
        dataInv <- x$get()
        s <- solve(dataInv, ...)
        x$setInv(s)
        ## Return a matrix that is the inverse of 'x', in this case, s
        s
}
