acc.rej<-function(n)
{
  x<-rep(0,n)
  i<-1
  repeat
  {
    U1<-runif(1,0,1)
    U2<-runif(1,0,1)
    fy<-(256/27)*U1*(1-U1)^3
    if(U2<=fy) 
    {
      x[i]<-U1
      i<-i+1
    }
    if(i>n) break
  }
  cat(x,"  ")
  hist(x)
}