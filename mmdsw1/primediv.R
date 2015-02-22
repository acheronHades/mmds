isprime <- function(num) {
  divs <- c()
  primedivs <- c(1, num)
  for (i in (1:num)) {
    if ((num/i)%%1 == 0)
      divs <- c(divs, i)
  }
  length(divs) == length(primedivs)
}

primediv <- function(num) {
  divs <- c(1)
  for (i in (2:num)) {
    if ((num/i)%%1 == 0 && isprime(i))
      divs <- c(divs, i)
  }
  divs
}

mmds1m <- function(num) {
  primes <- primediv(num)
  maplist <- list()
  for (i in primes) {
    pair <- list(c(i, num))
    maplist <- append(maplist, pair)
}
maplist
}

mmds1m2 <- function(vect = numeric()) {
  mapcomplete <- list()
  for (i in vect) {
    val <- mmds1m(i)
    mapcomplete <- append(mapcomplete, val)
  }
  mapcomplete
} 

mmds1r <- function(mlist = list()) {
  primes <- sapply(mlist, function(x) x[1])
  uprimes <- unique(primes)
  for (u in uprimes) {
    usum <- 0
    for (m in mlist) {
      if (m[1] == u)
        usum <- usum + m[2]
    }
    print(c(u, usum))
}
}

mmds1_sol <- function(intvect = numeric()) {
  map <- mmds1m2(intvect)
  reduce <- mmds1r(map)
  reduce
}