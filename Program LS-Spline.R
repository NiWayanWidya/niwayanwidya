library(readxl)
library(KernSmooth)

Data_Chlorine <- read_excel("Kelengkapan Kehidupan Kuliah/Semester V/Analisis Regresi Nonparametrik/Data Chlorine.xls", 
                            +     sheet = "Sheet2")
View(Data_Chlorine)
prediktor=Data_Chlorine$Weeks
respons=Data_Chlorine$Chlorine
data=cbind(prediktor,respons)
trun<-function(prediktor,knot,orde)
{
  prediktor[prediktor<knot]<-knot
  b<-(prediktor-knot)^orde
  return(b)
}
spline<-function(data)
{
  p<-as.numeric(readline("inputkan orde = "))
  k<-as.numeric(readline("inputkan jumlah knot = "))
  n<-nrow(data)
  prediktor<-data[,1]
  dataurut<-data[order(prediktor),1:2]
  x<-dataurut[,1]
  y<-dataurut[,2]
  w<-rep(0,k)
  cat("Jumlah Knot = ",k,"\n")
  for(i in 1:k)
  {
    cat("titik knots[",i,"] = ")
    w[i]<-as.numeric(readline(" "))
  }
  v1<-matrix(0,n,p+1)
  for(i in 1:p)
  {
    v1[,i]<-x^(i-1)
    v1[,(p+1)]<-x^(p)
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
    cat("\n Nilai betatopi[",i,"]=",format(betatopi[i]),"\n")
  }
  plot(x,y,pch=20,xlim=c(min(x),max(x)),
       ylim=c(min(c(y,ytopi)),max(c(y,ytopi))),xlab="Weeks",ylab="Chorine")
  par(new=T)
  plot(x,ytopi,type="l",col="red",lwd=8,xlim=c(min(x),max(x)),
       ylim=c(min(c(y,ytopi)),max(c(y,ytopi))),xlab="Weeks",ylab="Chorine")
  title(main="PLOT Weeks TERHADAP Chlorine")
  cat("Nilai MSE =",MSE,"\n")
  cat("Nilai GCV =",GCV,"\n")
  cat("Nilai R2 =",R2,"\n")
}
#contoh 6.2
spline(data)
