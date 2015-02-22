##mmds1_sol(c(15, 21, 24, 30, 49)) gives solution to question 4 in mmds week 4 quiz

isprime <- function(num) { ##checks if number is prime by looping through all divisors and comparing against vector c(1, num)
  divs <- c()
  primedivs <- c(1, num) ## since prime's only divisors are 1 and prime itself
  for (i in (1:num)) {
    if ((num/i)%%1 == 0)
      divs <- c(divs, i)
  }
  length(divs) == length(primedivs)
}

primediv <- function(num) { ## finds all prime divisors
  divs <- c(1)
  for (i in (2:num)) {
    if ((num/i)%%1 == 0 && isprime(i)) ## number has to be divisor and prime
      divs <- c(divs, i)
  }
  divs
}

mmds1m <- function(num) { ## first part of map returns pairs of numbers and primes 
  primes <- primediv(num) ## get prime divisors
  maplist <- list()
  for (i in primes) {
    pair <- list(c(i, num)) ## create pairs 
    maplist <- append(maplist, pair) ## create list of pairs
}
maplist
}

mmds1m2 <- function(vect = numeric()) { ## second part of map returns list of pairs of numbers and their primes for multiples values in vector vect
  mapcomplete <- list()
  for (i in vect) { ## for each number in vector call mmds1m
    val <- mmds1m(i)
    mapcomplete <- append(mapcomplete, val)
  }
  mapcomplete
} 

mmds1r <- function(mlist = list()) { ## reduce function 
  primes <- sapply(mlist, function(x) x[1]) ## get only the primes, i.e. the first element of each pair list 
  uprimes <- unique(primes) ## delete all redundancies
  for (u in uprimes) {
    usum <- 0
    for (m in mlist) { ## loop through the entire list and sum second elements of all lists for each prime
      if (m[1] == u)
        usum <- usum + m[2]
    }
    print(c(u, usum))
}
}

mmds1_sol <- function(intvect = numeric()) { ## solution 
  map <- mmds1m2(intvect) 
  reduce <- mmds1r(map)
  reduce
}