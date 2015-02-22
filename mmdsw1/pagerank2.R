pagerankhw2<- function() {
  ## compute pagerank as in homework quiz
  ## beta <- 0.7 ## question 1
  beta <- 0.85 ## question 2
  
  jump <- 1-beta
  ##M <- matrix(c(0,0,0,.5,0,0,.5,1,1), nrow = 3, ncol = 3) ## question 1; this is the column wise matrix of the pagerank functions for each node 
  M <- matrix(c(0,0,1,.5,0,0,.5,1,0), nrow = 3, ncol = 3) ## question 2; this is the column wise matrix of the pagerank functions for each node 
  
  r <- matrix(c(1/3,1/3,1/3))  ## the initial pagerank
  S <- r*jump 
  epsilon <- 1/10000
  r_old <- matrix() ## initiate empty matrix to compare against r
  difference <- 1   ## initiate difference to compare against epsilon
  
  while (difference > epsilon) { 
    r_old <-  r
    r <- beta * t(M)%*%r        ## matrix-vector product is %*% in R
    r <- r + S
    difference <- abs(max(r - r_old)) ## compute difference by just looking at the largest difference in any value between the two matrices
    ##print(difference)           ## just out of curiosity
  }
  
  a <- r[1,]
  b <- r[2,]
  c <- r[3,]
  
 ## this is poorly done, most of the equations in the test are == only when rounded. don't care that much, I just compared the output
  ##print(.95*b) 
  ##print(475*a + .05*c)
  print(.85*c == b + .575*a)
  print(.85*a == c + .15*b)
  print(c == .9*b + .475*a)
  print(.95*a == .9*c + .05*b)
  
}