mci_sin=function()
{
  cat("=======Monte Carlo ~ Integral=======")
  n=as.numeric(readline("Input banyak data x yang dibangkitkan :"))
  a=as.numeric(readline("Input batas bawah dis. uniform dari x:"))
  b=as.numeric(readline("Input batas atas dis. uniform dari x:"))
  x=runif(n,a,b)
  gx=rep(0,n)
  for(i in 1:n)
  {
    gx[i]=sin(x[i]) 
  }
  sumgx=sum(gx)
  i=((b-a)*sumgx)/n
  cat("\n Jumlah g(xi) = ",sumgx,"\n")
  cat("\n Nilai Integral = ",i,"\n")
}
mci_sin()


mcar=function()
{
  cat("=======Monte Carlo ~ Accept-Reject=======")
  n=as.numeric(readline("Input banyak data x yang dibangkitkan :"))
  a=as.numeric(readline("Input batas bawah dis. uniform dari x :"))
  b=as.numeric(readline("Input batas atas dis. uniform dari x :"))
  x=runif(n,a,b)
  fx=rep(0,n)
  for(i in 1:n)
  {
    fx[i] = (5*(x[i]^3)) + (7*x[i]) - 4
  }
  fmax = max(fx)
  y=runif(n,0,1)*fmax
  h=rep(0,n)
  for(i in 1:n)
  {
    if(y[i] < fx[i]) { h[i]=1 }
    else { h[i]=0 }
  }
  hasil = (sum(h)/n)*((b-a)*fmax)
  cat("\n Nilai Integral = ",hasil,"\n")
  
  result = data.frame(x,y,fx,h)
  acc=subset(result , h=="1")
  
  plot(x,y,pch=10)
  points(x,fx, col="black", lwd=2)
  points(acc[,1], acc[,2], col="purple", lwd=2)
  legend("topright", legend=c("reject","accept" ,"fx"), col=c("pink", "purple","black"), lty=1:1, cex=0.7,bg="white",lwd=3)
}
mcar()