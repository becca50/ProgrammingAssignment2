## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setInverse<-function(solve) inv<<- solve  
    getInverse<-function() inv  ##returns pointer to inverse
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse) #return list of function names
}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## check to see if the matrix has changed
    if(identical(x,x$get)){
        matrixSame=TRUE
    } else {
        matrixSame=FALSE
    }
    
    ## check to see if inverse is stored in cache
    inv <-x$getInverse()
    if (is.null(inv)) {
        cacheValue=FALSE  #if getInverse() returns NULL, the cache is empty.
    } else { cacheValue=TRUE}
    
    ## if matrix has changed OR the inverse is not in cache, solve for inverse.
    if(matrixSame==FALSE | cacheValue==FALSE){
        data<-x$get()
        inv<-solve(data, ...)
        x$setInverse(inv) 
        inv #return inverse matrix
    } else{
        message("getting cached data")
        inv #return cached inverse
    }
}