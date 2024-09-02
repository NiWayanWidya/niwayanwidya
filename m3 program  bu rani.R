RNGMultCongruen1<-function(a,x0,m,c,n)
{
  xn=rep(1,n)
  Un=rep(1,n)
  for (i in 1:n)
  {
    xn[i]=(a*x0+c)%%m  
    Un[i]=xn[i]/m
    x0=xn[i]
  }
  cat("xn   Un\n ")
  cat(xn," ",Un,"\n")
}

RNGMultCongruen2<-function(a,x0,m,c,n)
{
  cat("xn   Un\n ")  
  repeat
  {
    xn=(a*x0+c)%%m
    Un=xn/m
    cat(xn," ",Un,"\n")
    x0=xn
    n=n-1
    if( n<=0) break
  }
}

RNGMultCongruen3<-function(a,x0,m,c,n)
{
  cat("xn   Un\n ")  
  while (n>0) {
    xn=(a*x0+c)%%m
    Un=xn/m
    cat(xn," ",Un,"\n")
    x0=xn
    n<-n-1
  }
}
