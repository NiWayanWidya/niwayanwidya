#NAMA : Ni Wayan Widya Septia Sari
#NIM  : 082011833050

#UTS STAT KOM PRAK S2 
#NO 3
iterasijacobi <- function(a, b, tol=1e-7, maxiter=100){
  n <- length(b)
  iter <- 0
  
  Dinv <- diag(1/diag(a))
  R <- a-diag(diag(a))
  x <- rep(0,n)
  x_new <- rep(tol, n)
  
  while(sqrt(sum(x_new-x)^2)>tol){
    if(iter>maxiter){
      warning("melebihi batas iterasi maksimum")
      break
    }
    x <- x_new
    x_new <- Dinv %% (b - R %% x)
    iter <- iter+1
  }
  return(list(X = x_new, iter=iter))
  
}

A <- matrix(c(27,6,1,6,15,1,-1,2,54), 3)
b <- c(85,72,110)
A
b
iterasijacobi(A,b,0.0000001,30)