pagerank3 <- function(a,b,c) {
  ## compute the pagerank for three sites in one iteration
  beta <- 0.7
  anew <- 0*beta + (a+b+c)*(1-beta)
  bnew <- (a/2)*beta + (a+b+c)*(1-beta)
  cnew <- (a/2 + b + c)*beta + (a+b+c)*(1-beta)
  c(anew, bnew, cnew)
}

pagerankhw<- function() {
  ## compute pagerank as in homework quiz
  beta <- 0.7
  jump <- 1-beta
  M <- matrix(c(0,0,0,.5,0,0,.5,1,1), nrow = 3, ncol = 3)
  r <- matrix(c(1/3,1/3,1/3))
  S <- r*jump
  epsilon <- 1/10000
  
  
  power_iteration <- function(r) {
    r_old <-  r
    r <- beta * t(M)%*%r
    r <- r + S
    difference <- abs(max(r - r_old))
    
  }
  
  homework <- function() {
    power_iteration()
    
  }
}