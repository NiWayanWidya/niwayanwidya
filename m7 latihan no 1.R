a<-2 #shape parameter
u<-rgamma(1,shape=a,rate=1)
u

b<-3 #shape parameter
v<-rgamma(1,shape=b,rate=1)
v

rgamma_a <- function(a) {
  Z <- rexp(a)
  Y <- sum(Z)
  return
}

rgamma_b <- function(b) {
  Z <- rexp(b)
  W <- sum(Z)
  return(W)
}

rbeta_rs <- function(r, s) {
  U <- rgamma_a(r)
  V <- rgamma_b(s)
  x<-u/(u+v)
  return(X)
}


rbeta_rs<-function(r,s) {
  u<-rgamma_a(r)
  v<-rgamma_b(s)
  x<-u/(u+v)
  print(x)
}

set.seed(123)
rbeta_rs(2, 3)
sapply(1:10, function(x) rbeta_rs(2, 3))


