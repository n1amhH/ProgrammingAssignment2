## makeCacheMatrix holds the getters and setters for the matrix x and its 
## inverse i 
## the set  clears the cached inverse when a new makeCacheMatrix object is 
## instantiated. 
##cacheSolve  first checks the cache to see if there is a value for i  in the 
##cache and if so it returns that value otherwise if calculates the inverse 
## and sets it via setinverse and returns that value.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        ## For this assignment, we may assume that the matrix supplied is always
        ## invertible. Therefore, no checking needs to be done on the structure 
        ## of the matrix.
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
                
        }
        get<-function() x
        
        setinverse<-function(inverse) i<<-inverse
        getinverse<-function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setinverse(i)
        i
}
