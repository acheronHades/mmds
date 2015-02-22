pagerankhw3<- function() {
  beta <- 1 ## question 3
  
  jump <- 1-beta
  M <- matrix(c(0,0,1,.5,0,0,.5,1,0), nrow = 3, ncol = 3) ## question 3; this is the column wise matrix of the pagerank functions for each node 
  
  r <- matrix(c(1,1,1))  ## the initial pagerank a=b=c=1 in question 3
  S <- r*jump 
  epsilon <- 1/10000
  r_old <- matrix() ## initiate empty matrix to compare against r
  difference <- 1   ## initiate difference to compare against epsilon
  i <- 0
  
  while (difference > epsilon) { 
    r_old <-  r
    r <- beta * t(M)%*%r        ## had to research this: matrix-vector product is %*% i R
    r <- r + S
    difference <- abs(max(r - r_old))
    print('iteration #')
    print(i)
    i <- i +1
    print(r)           ## to see values for solution
    print('-----')
  }

  
}