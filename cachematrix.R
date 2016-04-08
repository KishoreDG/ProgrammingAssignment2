# Goal of this assignment is demonstrate how store values 
# using <<- operator which can be used to assign a value to an object in 
# an environment that is different from the current environment. 
# 

#> source("<your local path>/ProgAssign2.R")

#> a <- makeCacheMatrix(matrix(c(2,2,4,6),nrow=2,ncol=2))

#> fcacheSolve(a)
#[,1] [,2]
#[1,]  1.5 -1.0
#[2,] -0.5  0.5

#> fcacheSolve(a)
#[,1] [,2]
#[1,]  1.5 -1.0
#[2,] -0.5  0.5

 
makeCacheMatrix <- function(fValue = matrix())
{
  
  mCache <- NULL  ## declare a variable to hold the cache
  ## set it to nothing (NULL)
  
  ## function set matrix data 
  fSetMatrix <- function(mValue)
  {
    fValue <<- mValue ## new mValue is now initialized to "fValue"
    mCache <<- NULL   ## clear the data in cache variable 
  }
  
  ## function to get the data from fValue
  fGetMatrix <- function()
  {
    fValue
  }
  
  # set
  fSetToCache <- function(s)
  {
    mCache <<- s
  }
  
  fgetFromCache <- function()
  {
    mCache
  }
  
  list(fSetMatrix = fSetMatrix, fGetMatrix = fGetMatrix, fSetToCache = fSetToCache, fgetFromCache = fgetFromCache)
  
}


## Solve Operation Function

fcacheSolve <- function(y, ...) {
  
  # try to get the value from Cache using fGetInverseFromCache function 
  mInverse <- y$fgetFromCache()
  
  # make sure that there is a valid value in the cache (check for not NULL)
  # if value persists in the cache
  # return it
  
  if(!is.null(mInverse)) {
    return(mInverse)
  }
  
  # if no previous value exists in the cache
  # caclulate the inverse for the matrix (this could be the first time as well)
  # once the calculation is done, store it in the cache 
  # using fSetInverseToCache function
  
  mData <- y$fGetMatrix()
  mInverse <- solve(mData)     ## solve function is used
  y$fSetToCache(mInverse)
  
  # return the calculated inverse
  mInverse
}