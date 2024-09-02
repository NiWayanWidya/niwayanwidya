multi=function()
{
  x0=as.numeric(readline("Nilai X0 = "))
  a=as.numeric(readline("Nilai a = "))
  m=as.numeric(readline("Nilai m = "))
  n=as.numeric(readline("Banyak bilangan yang diinginkan = "))
  z=rep(0,(n+1))
  u=rep(0,n)
  z[1]=x0
  for(i in 1:n)
  {
   z[i+1]=(a*z[i])%%m
   u[i]=z[i+1]/m
   cat("Z[",i-1,"] = ",z[i+1],"\n")
   cat("Bilangan acak ke-",i," = ", u[i],"\n")
  }
}
multi()

mixed=function()
{
  x02=as.numeric(readline("Nilai X0 = "))
  a2=as.numeric(readline("Nilai a = "))
  m2=as.numeric(readline("Nilai m = "))
  c2=as.numeric(readline("Nilai c = "))
  n2=as.numeric(readline("Banyak bilangan yang diinginkan = "))
  x2=rep(0,(n2+1))
  u2=rep(0,n2)
  x2[1]=x02
  for(i in 1:n2)
  {
    x2[i+1]=(a2*x2[i]+c2)%%m2
    u2[i]=x2[i+1]/m2
    cat("x[",i-1,"] = ",x2[i+1],"\n")
    cat("Bilangan acak ke-",i," = ", u2[i],"\n")
  }
}
mixed()
