TLChisquare=function()
{
  n=as.numeric(readline("Input banyak U : "))
  x=runif(n)
  z=rep(0,n)
  z2=rep(0,n)
  for(i in 1:n)
  {
    z[i]=qnorm(x[i],0,1)
    z2[i]=z[i]**2
  }
  print(cbind(z,z2))
  hist(z, main="Histogram dari Z~N(0,1)", col="yellow")
  hist(z2, main="Histogram dari Z^2~X^2(1)", col="green")
}
TLChisquare()

TLF=function()
{
  a=as.numeric(readline("Input banyak U : "))
  m=as.numeric(readline("Input derajat bebas dari U : "))
  n=as.numeric(readline("Input derajat bebas dari  v : "))
  u=rchisq(a,m)
  v=rchisq(a,n)
  f=rep(0,a)
  for(i in 1:a)
  {
    f[i]=(u[i]/m)/(v[i]/n)
  }
  print(cbind(u,v,f))
  hist(u, main="Histogram dari U~X^2(m)", col="yellow")
  hist(v, main="Histogram dari V~X^2(n)", col="green")
  hist(f, main="Histogram dari F~F(m,n)", col="pink")
}
TLF()

