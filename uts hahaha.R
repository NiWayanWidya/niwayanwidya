library(KernSmooth)
library(locpol)
library(MASS)
library(readxl)
library(pspline)
imt<- read_excel("C:/Users/widyaseptia/Downloads/DATA UTS 2022.xls")

#no1
prediktor = imt$prediktor
respons = imt$respon2
data=cbind(prediktor,respons)
h = pluginBw(prediktor, respons, 1, kernel = CosK)
h
estimasiLoclin=locLinSmootherC(prediktor,respons,5,h,kernel= CosK)
estimasiLoclin

#no2
prediktor = imt$prediktor
respons = imt$respon4
h = regCVBwSelC(prediktor, respons, 0, kernel = gaussK)
h

estimasiLoclin=locLinSmootherC(prediktor,respons,0,h,kernel= CosK)
estimasiLoclin

estimasilockons=locPolWeights(prediktor,0,h,kernel= gaussK)
estimasiLockons

#no3
library(stats)
library(npreg)
library(pspline)
prediktor<-DATA_UTS_2022$prediktor
respons<-DATA_UTS_2022$respon3
data=cbind(prediktor,respons)
dataurut<-data[order(prediktor),1:2]
x<-dataurut[,1]
y<-dataurut[,2]

smoothing.spline = smooth.spline(x,y) #x nya usahakan udah yang terurt
smoothing.spline

estimasirespon= smooth.Pspline(x,y,5)
estimasirespon

mod.smsp<-smooth.spline(x,y,df=19)
library(npreg)
mod.smoothing.spline<-smooth.spline(x,y,df=19)
mod.smoothing.spline
mean((mod.smoothing.spline$y-mod.smsp$y)^2)

prediktor<-DATA_UTS_2022$prediktor
respons<-DATA_UTS_2022$respon3
data=cbind(prediktor,respons)
estimasirespon(5)

#no4
library(readxl)
DataImport<-read_excel("C:/Users/widyaseptia/Downloads/DATA UTS 2022.xls")
prediktor=DataImport$prediktor
respons=DataImport$respon1
data=cbind(prediktor,respons)
spline<-function(respon1)
{
  p<-as.numeric(readline("inputkan orde = "))
  k<-as.numeric(readline("inputkan jumlah titik knot optimal= "))
  n<-nrow(data)
  prediktor<-data[,1]
  dataurut<-data[order(prediktor),1:2]
  x1<-dataurut[,1]
  y1<-dataurut[,2]
  x<-as.matrix(x1)
  y<-as.matrix(y1)
  w<-rep(0,k)
  cat("Jumlah knot = ",k,"\n")
  for(i in 1:k)
  {
    cat("titik knot = ")
    w[i]<-as.numeric(readline(" "))
    knot<-w[i]
  }
  v1<-matrix(0,n,p+1)
  for(i in 1:p)
  {
    
    v1[,i]<-x^(i-1)
    v1[,p+1]<-x^(p)
  }
  v2<-matrix(0,n,k)
  for(i in 1:k)
  {
    v2[,i]<-trun(x,w[i],p)
  }
  X<-cbind(v1,v2)
  betatopi<-solve(t(X)%*%X)%*%t(X)%*%y
  ytopi<-X%*%betatopi
  H<-X%*%solve(t(X)%*%X)%*%t(X)
  MSE<-(t(y-ytopi)%*%(y-ytopi))/n
  GCV<-MSE/(1-((1/n)*sum(diag(H))))^2
  JKT<-t(y-(mean(y)))%*%(y-(mean(y)))
  JKG<-t(y-ytopi)%*%(y-ytopi)
  R2<-1-(JKG/JKT)
  for(i in 1:(p+1+k))
  {
    cat("\n nilai betatopi[ ",i,"]= ",format(betatopi[i]),"\n")
  }
  win.graph()
  plot(x,y,xlim=c(min(x),max(x)),ylim=c(min(c(y,ytopi)),max(c(y,ytopi))),xlab="Weeks",ylab="Chlorine")
  par(new=T)
  plot(x,ytopi,type="l",col="red",xlim=c(min(x),max(x)),ylim=c(min(c(y,ytopi)),max(c(y,ytopi))),xlab="Weeks",ylab="Chlorine")
  title(main="Plot Weeks terhadap Chlorine")
  cat("	Nilai MSE = ",MSE,"\n")
  cat("	Nilai GCV = ",GCV,"\n")
  cat("	Nilai R2  = ",R2,"\n")
}
spline(respon1)

