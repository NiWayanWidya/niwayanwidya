RNGMultCongruen1<-function(a,x0,m,n)
{
  xn=rep(1,n)
  Un=rep(1,n)
  for (i in 1:n)
  {
    xn[i]=(a*x0)%%m  
    Un[i]=xn[i]/m
    x0=xn[i]
  }
  cat("xn   Un\n ")
  cat(xn," ",Un,"\n")
}

RNGMultCongruen2<-function(a,x0,m,n)
{
  cat("xn   Un\n ")  
  repeat
  {
    xn=(a*x0)%%m
    Un=xn/m
    cat(xn," ",Un,"\n")
    x0=xn
    n=n-1
    if( n<=0) break
  }
}

RNGMultCongruen3<-function(a,x0,m,n)
{
  cat("xn   Un\n ")  
  while (n>0) {
    xn=(a*x0)%%m
    Un=xn/m
    cat(xn," ",Un,"\n")
    x0=xn
    n<-n-1
  }
}

multi=function(x0,a,m,n){
  x=rep(0,n)
  u=rep(0,n)
  x[1]=(a*x0) %% m
  u[1]=x[1]/m
  for (i in 2:n)
  {
    x[i]=(a*x[i-1]) %% m
    u[i]=x[i]/m
  }
  print(u)
}