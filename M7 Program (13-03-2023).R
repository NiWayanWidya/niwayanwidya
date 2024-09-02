normalCLT=function()
{
  n=as.numeric(readline("Berapa data yang akan dibangkitkan ="))
  m=as.numeric(readline("Berapa data yang akan dijumlahkan ="))
  miu=as.numeric(readline("Berapa nilai miu ="))
  varian=as.numeric(readline("Berapa nilai varian ="))
  a=rep(0,n)
  for(i in 1:n)
  {
    b=runif(m,0,1)
    N=(sum(b)-m/2)/sqrt(m/12)
    a[i]=N
  }
  print(a)
  sigma=sqrt(varian)
  X=(a*sigma)+miu
  plot(density(X),xlab="x",ylab="p",col="yellow",lwd=3)
  x=seq(min(X),max(X),0.1)
  y=dnorm(x,miu,sigma)
  lines(x,y,col="blue",lwd=3)
}
normalCLT()

#=============================================

normalBox=function()
{
  n=as.numeric(readline("Berapa data yang akan dibangkitkan ="))
  miu=as.numeric(readline("Berapa nilai miu ="))
  varian=as.numeric(readline("Berapa nilai varian ="))
  u1=runif(n,0,1)
  u2=runif(n,0,1)
  N1=sqrt(-2*log(u1))*cos(2*pi*u2)
  N2=sqrt(-2*log(u1))*sin(2*pi*u2)
  N=c(N1,N2)
  sigma=sqrt(varian)
  X=(N*sigma)+miu
  plot(density(X),xlab="x",ylab="p",col="red",lwd=3)
  x=seq(min(X),max(X),0.1)
  y=dnorm(x,miu,sigma)
  lines(x,y,col="blue",lwd=3)
}
normalBox()

#=======================

chisq=function()
{
  n=as.numeric(readline("Berapa data yang akan dibangkitkan ="))
  r=as.numeric(readline("Berapa nilai derajat bebas ="))
  X2=rep(0,n)
  for(i in 1:n)
  {
    u1=runif(r/2,0,1)
    u2=runif(r/2,0,1)
    N1=sqrt(-2*log(u1))*cos(2*pi*u2)
    N2=sqrt(-2*log(u1))*sin(2*pi*u2)
    N=c(N1,N2)
    Y=N^2
    X2[i]=sum(Y)
  }
  plot(density(X2),xlab="x",ylab="p",col="yellow",lwd=3)
  x=seq(min(X2),max(X2),0.1)
  y=dchisq(x,r)
  lines(x,y,col="blue",lwd=3)
}
chisq()

#===========================

t=function()
{
  n=as.numeric(readline("Berapa data yang akan dibangkitkan ="))
  r=as.numeric(readline("Berapa nilai derajat bebas ="))
  X2=rep(n,0)
  for(i in 1:n)
  {
    u1=runif(r/2,0,1)
    u2=runif(r/2,0,1)
    N1=sqrt(-2*log(u1))*cos(2*pi*u2)
    N2=sqrt(-2*log(u1))*sin(2*pi*u2)
    N=c(N1,N2)
    Y=N^2
    X2[i]=sum(Y)
  }
  t=plot(density(X2),xlab="x",ylab="p",col="red",lwd=3)
  x=seq(min(X2),max(X2),0.1)
  y=dchisq(x,r)
  lines(x,y,col="blue",lwd=3)
}
t()

#====================================

F=function()
{
  n=as.numeric(readline("Berapa data yang akan dibangkitkan ="))
  r=as.numeric(readline("Berapa nilai derajat bebas ="))
  X2=rep(0,n)
  for(i in 1:n)
  {
    u1=runif(r/2,0,1)
    u2=runif(r/2,0,1)
    N1=sqrt(-2*log(u1))*cos(2*pi*u2)
    N2=sqrt(-2*log(u1))*sin(2*pi*u2)
    N=c(N1,N2)
    Y=N^2
    X2[i]=sum(Y)
  }
  plot(density(X2),xlab="x",ylab="p",col="red",lwd=3)
  x=seq(min(X2),max(X2),0.1)
  y=dchisq(x,r)
  lines(x,y,col="blue",lwd=3)
}
F()