## Programming assignment in Coursera R programming course

## Creates a special kind of matrix(/list), which caches matrix inverses. 
## Returns a list, which has the cached result
makeCacheMatrix <- function(x = matrix()) {
		cache<- NULL ## the cached matrix inverse
		
		## function to set the matrix
		set <- function(y) {
			x<<-y
			cache<<-NULL ## cache is nulled, because matrix has changed
		}
		
		## function to get matrix
		get <- function() x
		
		setinv <- function(inv) cache<<-inv ## setting inverse cache
		getinv <- function() cache ## returning the cached inverse
		list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## Function returns the inverse of matrix x. It either return cached inverse (if available), or calculates it through solve 
## If inverse is calculated, it is also cached as part of CacheMatrix.
cacheSolve <- function(x, ...) {
        
		m<-x$getinv()
		if(!is.null(m)) {
			message("getting cached data")
			return(m)  ## inverse is cached
		}
		
		## inverse is not cached, so we calculate matrix inverse,
		## cache it, and finally return it
		data<-x$get()
		m<-solve(data,...)
		x$setinv(m)
		m
}
