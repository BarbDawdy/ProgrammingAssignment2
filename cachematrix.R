

## this function is to cache the inverse of a square matrix (refer to linear algebra if help needed http://mathworld.wolfram.com/MatrixInverse.html)
## this function is based on the provided makeVector.R but using the inverse function NOT mean function
##   1. set the matrix
##   2. get the matrix
##   3. set the inverse matrix
##   4. get the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  # 1. set the matrix
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  #2. get the matrix
  get <- function() x
  #3. set the inverse matrix
  setInverseMat <- function(inverse) mInv <<- inverse
  #4. get the inverse matrix
  getInverseMat <- function() mInv
  list(set = set, get = get,
       setInverseMat = setInverseMat,
       getInverseMat = getInverseMat)
}


## this function is to return the inverse of a cashed matrix
## this function is based on the provided cachemean.R
##   
## test data by creating a simple test below or a big matrix of random data rMat = rnorm(10000000) and then a matrix with say matTst=matrix(rMat, nrow=1000, ncol=1000)
## m = makeCacheMatrix(matTst) and then cacheSolve(m)...warning this will take a long time first time...might test with simple 2x2 mat below.
## then call makeCacheMat(matTst) and then cacheSolve....then do again just to see how it works
## maybe a test program with some timing would be cool to see if the caching makes it faster  ;)
## make sure input is a square matrix.
##
## simple test with variable prints to see data is getting matrix inversed
## x = rbind(c(1, 2), c(85, 42))
## x
##      [,1] [,2]
##[1,]    1    2
##[2,]   85   42
##
## m = makeCacheMatrix(x)
## Now run cache solve first time
## cacheSolve(m)
##[,1]       [,2]
##[1,] -0.3281250  0.0156250
##[2,]  0.6640625 -0.0078125
## Now run chache solve again....
##[1] "inverse done already = getting cashed matrix"
##[,1]       [,2]
##[1,] -0.3281250  0.0156250
##[2,]  0.6640625 -0.0078125
## super simple test done...do big test on big matrix and see time delay or write a test program callign sys.time() to output timing
## output shows proper inverse of square matrix.
##


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mInv = x$getInverseMat()
  
  #check that inverse of matrix has not already been processed
  if (!is.null(mInv)){
    #print info message so we know the cashed data is being pulled and skiping inversing matrix and return the cashed inverse matrix
    print(mInv)
    print("inverse done already = getting cashed matrix")
    return(mInv)
  }
  # if matrix is not inversed yet then work needs to be done to inverse matrix and cache
  mat.data <- x$get()
  mInv <- solve (mat.data, ...)
  
  #now cache the matrix for the future with setInverseMat and return the inversed matrix
  #print there but now commented out for testing purposes
  x$setInverseMat(mInv)
  mInv
}
