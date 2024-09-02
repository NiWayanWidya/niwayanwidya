fungsi_diskrit<-function(n)
{
  p1<-0.20
  p2<-0.15
  p3<-0.25
  p4<-0.40
  p12<-p1+p2
  p123<-p1+p2+p3
  u<-rep(0,n)
  for (i in 1:n) {
    random<-runif(1, min = 0, max = 1) 
    u[i]<-random
    if (u[i] < p1) cat(u[i]," ",1,"\n")
    else if (u[i] < p12) cat(u[i]," ",2,"\n")
    else if (u[i] < p123) cat(u[i]," ",3,"\n")
    else cat(u[i]," ",4,"\n")
  }
}
fungsi_diskrit(10)

fungsi_kontinu_eksponensial<-function(n)
{
  u=runif(n, min = 0, max = 1)
  x=-log(1-u)
  cat(u,"\n")
  cat(x)
}
fungsi_kontinu_eksponensial(5)
