## Put comments here that give an overall description of what your
##The first function will creat a special matrix, which really is a list cointaining 
##a function to 1. Set value of the matrix, 2. Get the value of the matrix, 
##3. Set the value of the inverse matrix, 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get<- function(){x}
    setinv <- function(inverse){ i <<- inverse }
    getinv <- function(){ i }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##The following function calculates the inverse of the special "matrix" created with
##the above function. However, it first checks to see if the inverse has already been
##calculated. If so, it gets the inverse of the data and sets the value of the inverse
##in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
    inv<-x $ getinv()
    if(!is.null(inv)){
        message("getting cahed data")
        return(inv)
    }
    data<-x $ get()
    inv<-solve(data,...)
    x $ setinv(inv)
    inv
}
