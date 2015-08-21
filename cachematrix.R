## makeCacheMatrix and cacheSolve together allow for creating a cached value for
## the inverse of a matrix.

## makeCacheMatrix caches the inverse of the matrix, which will default to NULL
## makeCacheMatrix also saves a cache of the original matrix


makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<- function() x
        setinv<-function(invert) inv<<-invert
        getinv<-function() inv
        list(set=set, get=get, 
             setinv=setinv, getinv=getinv)

}


## cacheSolve determines if a cached version of the inverse of the matrix exists
## if so, it grabs the cached version and returns it. If not it calculates the
## inverse and feeds it back into the makeCacheMatrix function to cache it for 
## future use. Ultimately this returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ##this pulls the inv from makeCacheMatrix
        inv<- x$getinv
        data<- x$get
        if ((!is.null(inv)) & (x==data)) {
                message ("getting cached inverse")
                return(inv)                        
        }
        ## above checks if the inverse is cached. if it is AND the matrix used
        ## is the same, it returns the inverse from cached
        data<-x$set
        inv<-solve(data, ...)
        x$setinv(inv)
        inv

}
