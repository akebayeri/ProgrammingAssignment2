## These two functions allow me to store the inverse of a matrix in a cache
## and load it whenever needed.
## 

## This function takes as parameters a matrix and contains a list of function 
##to set ,get, and calculate the invers of a matrix

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL 
        set <- function(y){
                x <<- y
                m<<-NULL
                
        }
        get <- function() x
        setmyinv <- function(myinv) m<<- myinv
        getmyinv <- function()m
        list(set =set,get = get, setmyinv = setmyinv, getmyinv = getmyinv)

}


## This function get as parameters a matrix and checks 
## if the inverse of this matrix has been stored in memory
## if not its calculate the inverse and cache it . If the inverse is 
## laready calculated it will just retreive it from the cache

cacheSolve <- function(x, ...) {
        
        m <- x$getmyinv()
        if(!is.null(m)){
                message("Loading from the cache")
                return(m)
        }
        inv <- x$get()
        m <- solve(inv, ...)
        x$setmyinv(m)
        m
}
