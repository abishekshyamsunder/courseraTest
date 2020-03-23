## The functions below serve to store the inverse of the matrix in cache so as to reduce cost of computation

## The makeCacheMatrix function takes a matrix as argument and creates an object (which is a list), which contains the matrix itself along with its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  get <- function() x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function() inv
  list(mat = x,get = get ,setinv = setinv, getinv = getinv)
}
## The cacheSolve function takes this created object as argument, and returns the inverse if already exists, else calculates and returns
cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(is.null(inv))
  {
    print("Inverse not cached, Calculating...")
    mat<-x$get()
    inv<-solve(mat)
    x$setinv(inv)
    inv
  }
  else
    return(inv)
}
## uncomment the below 3 lines of code to run an example with the identity matrix as input 
t<-matrix(c(1,0,0,0,1,0,0,0,1),nrow = 3,ncol=3)
tt<-makeCacheMatrix(t)
cacheSolve(tt)
## Notice that the second time the cacheSolve(tt) function is called, the 'Calculating' line is not printed
cacheSolve(tt)
