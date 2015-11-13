## Put comments here that give an overall description of what your
## functions do

#the makeCachematric function below creates a list containing a function that set and get the value of the matrix
#This function also set and get the inverse of the matrix
makeCacheMatrix<- function(x=matrix())
{
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
    
  }
get<-function() x
  setInverseMatrix<-function(solve) m <<- solve
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  }
# the cacheInverseMatric function will skip the computation if the inverse of the matrix is available in the cache,if not 
# it will compute it properly
cacheInverseMatric <- function(x, ...) {
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getInverseMatrix()
  m <- solve(data, ...)
  x$setInverseMatrix(m)
  m
}
