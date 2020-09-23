## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix"
##object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    mati<- NULL
    set <-function(y){
      x<<-y
      mati<<-NULL
      
    }
    get<-function(){x}
    setInv<-function(inverse)mati<<- inverse
    getInv<-function()mati
    list(set=set,get=get,
         setInv=setInv,
         getInv=getInv)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache.

cacheSolve<-function(x,...){
  ## Return a matrix that is the inverse of 'x'
  mati<-x$getInv()
  if(!is.null(mati)){
    message("getting cached data")
    return(mati)
  }
  mat <-x$get()
  mati<-solve(mat,...)
  x$setInv(mati)
  mati
} 

