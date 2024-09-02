library(readxl)
library(KernSmooth)

prediktor=chlorine$Weeks
respons=chlorine$Chlorine
prediktor=seq(1,9,1)
prediktor
quant<-function(prediktor,P)
{
  r<-quantile(prediktor,seq(0,1,1/2))
  return(r)
}
spline<-function(data)
{
  p<-as.numeric(readline("inputkan orde = "))
  k<-as.numeric(readline("inputkan jumlah knot = "))
  bb<-as.numeric(readline("inputkan batas bawah lamda= "))
  ba<-as.numeric(readline("inputkan batas atas lamda= "))
  h<-as.numeric(readline("increament lamda= "))
  vlamda<- seq(bb,ba,h)
  nvlamda<- length(vlamda)
  GCV<- rep(0,nvlamda)
  w<- rep(0,k)
  cat("jumlah knot= ",k,"\n")
  for(i in 1:k)
  {
    cat("titik knot[",i,"]=")
    w[i]<-as.numberic(readline(""))
  }
for(r in 1:nvlamda)
{
  n<-nrow(data)
  prediktor<-data[,1]
  dataurut<-data[order(prediktor),1:2]
  x<-dataurut[,1]
  
}
