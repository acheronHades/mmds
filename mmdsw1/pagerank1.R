pagerankhw1<- function() {
  ## compute pagerank as in homework quiz
  beta <- 0.7 ## question 1
  
  jump <- 1-beta
  M <- matrix(c(0,0,0,.5,0,0,.5,1,1), nrow = 3, ncol = 3) ## question 1; this is the column wise matrix of the pagerank functions for each node 
  
  r <- matrix(c(1/3,1/3,1/3))  ## the initial pagerank
  S <- r*jump 
  epsilon <- 1/10000
  r_old <- matrix() ## initiate empty matrix to compare against r
  difference <- 1   ## initiate difference to compare against epsilon
  
  while (difference > epsilon) { 
    r_old <-  r
    r <- beta * t(M)%*%r        ## had to research this: matrix-vector product is %*% i R
    r <- r + S
    difference <- abs(max(r - r_old))
    ##print(difference)           ## just out of curiosity
  }
  ##question 1
  rnew <- r*3
  print(rnew[1,]+rnew[3,])
  print(rnew[1,]+rnew[2,])
  print(rnew[2,]+rnew[3,])
  
 
}