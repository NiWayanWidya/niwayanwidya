M<-1000 ; theta <- 0.36 ;
# Generate independent uniform random variables U and V
U <- runif(M, theta);
u
V <- runif(M, theta);
v
X <- (1 + log(V)) / log(1 - (1 - theta)^U);


Penyelesaian <- function(M, theta) {
  U <- runif(M, theta)
  V <- runif(M, theta)
  # Calculate X using the formula
  X <- (1 + log(V)) / log(1 - (1 - theta)^U)
  print(x)
}
Penyelesaian(1000, 0.36)