B <- matrix(c(1,2,3,4),2,2)
#solve(B) #We pretend that this cant't happen xD
B1 <- makeCacheMatrix(B)
cacheSolve(B1) #inverse returned after computation
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
cacheSolve(B1) #inverse returned from cache
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5