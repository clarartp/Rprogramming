# makeCachematrix
# Create a special matrix that caches its inverse 
# as follows:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix<- function (x=matrix()) {
  i<-NULL
  set<-function(y)
  {
    x<<-y
    i<<-NULL
  }
get<-function() x
setinverse<-function(inverse) i<<-inverse
getinverse<-function() i
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

#Cachesolve  will compute the inverse of the matrix returned 
#by  the above function makeCacheMtrix. In the case the inverse 
#has already been calculated will get the value from the cache and 
#skip the computation. Otherwise, it will compute it
# set it in the cache via the setinverse function.

cachesolve<-function(x,...)
{
  i<-x$getinverse()
  if(!is.null(i))
  {
    return(i)
  }
  i<-solve(x$get())
  x$setinverse(i)
  i
}
  
