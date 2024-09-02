#NAMA : Ni Wayan Widya Septia Sari
#NIM  : 082011833050

#TUGAS BAB 6 
#NO1
NO1<- function(n,p)
{
  x=0
  cat("TABEL NILAI PROBABILITAS VARIABEL RANDOM BINOMIAL")
  cat("-------------------------------------------------\n")
  cat("          x                 f(x)                ")
  cat("-------------------------------------------------\n")
  for(i in 0:n){
    fx<- round(dbinom(x,n,p),5)
    cat("  ",x," ",fx,"\n")
    x<- x+1
  }
}
NO1(10, 0.3)

#NO2
NO2<- function()
{
  sum<- 0
  for(i in 1:40){
    cat("+-")
  }
  cat("\n")
  cat("TABEL NILAI PROBABILITAS VARIABEL RANDOM BINOMIAL")
  n=as.numeric(readline("Masukkan ukuran (n) : "))
  p=as.numeric(readline("Masukkan nilai probabilititas : "))
  for(i in 1:80){
    cat("-")
  }
  cat("\n")
  cat("\t x \t p \t\t\t p(x) \n")
  for(i in 1:80){
    cat("-")
  }
  cat("\n")
  for(i in 0:n){
    y<- round(dbinom(i,n,p),8)
    sum<- round((sum+y),8)
    cat("t",i,"\t",y,"\t\t",sum,"\n")
  }
}
NO2()

#NO3
NO3<- function(x,n,pe)
{
  vecx=rep(0,n)
  vecp=rep(0,n)
  cat("---------------\n")
  cat("TABEL BINOMIAL")
  cat("---------------\n")
  k=0
  while(k<=n){
    p=dbinom(x,n,pe)
    vecx[k]=x
    vecp[k]=p
    cat(" ",x,"|",p,"\n")
    x=x+1
    k=k+1
  }
  cat("Nilai x yang membuat probabilitas maksimum : ",vecx [which.max(vecp)],"\n")
}
NO3(0,10,0.25)

#NO4
NO4<-function(mean,sd)
{
  a=as.numeric(readline("Batas bawah x : "))
  b=as.numeric(readline("Batas atas x : "))
  i=as.numeric(readline("Increment : "))
  n=(b-a)/i
  vecx=rep(0,n)
  vecfx=rep(0,n)
  cat("Tabel PDF Distribusi Normal\n")
  cat("---------------------------------\n")
  cat("   x               f(x)\n")
  cat("---------------------------------\n")
  x=a
  for(k in 0:n)
  {
    k=k+1
    pdf=dnorm(x,mean,sd)
    vecx[k]=x
    vecfx[k]=pdf
    cat("  ",x,"          ",pdf,"\n")
    x=x+i
  }
  plot(vecx,vecfx,type="l")
}
no4()

#NO5
NO5<-function(L)
{
  BB=L[,1]
  BA=L[,2]
  fr=L[,3]
  P=L[1,2]-L[1,1]+1
  k=nrow(L)
  xi=(BB+BA)/2
  
  #A.mean
  mean=sum(fr*xi)/sum(fr)
  cat("rata rata :",mean)
  
  #B.Varians
  var=sum(fr*((xi-mean)^2))/(sum(fr)-1)
  cat("\n Varians :",var)
  
  #C.Median
  fk=cumsum(fr)
  for(i in 1:k)
  {
    if(sum(fr)/2<=fk[i])
    {
      medc=i
      break
    }
  }
  cat("\n Median terletak pada kelas ke ;",medc)
  M=L[medc,1]-0.5
  med=M+(P*((sum(fr)/2)-fk[medc])/fr[medc])
  cat("\n Median :",med)
  
  #D.modus
  for(i in 1:k)
  {
    if (max(fr)<=fr[i])
    {modc=i
    break
    }
  }
  cat("\n Modus terletak pada kelas ke :",modc)
  M=L[modc,1]-0.5
  d1=fr[modc]-fr[modc-1]
  d2=fr[modc]-fr[modc+1]
  modus=M+((P*d1)/(d1+d2))
  cat("\n Modus :",modus)
}
BB=c(52,57,62,67,72,77,82)
BA=c(56,51,66,71,76,81,86)
fr=c(5,3,15,6,11,7,3)
L=cbind(BB,BA,fr)
L
NO5(L)

#NO6
NO6<- function()
{
  for(i in 1:40){
    cat("+-")
  }
  cat("\n")
  cat("TABEL NILAI PROBABILITAS VARIABLE RANDOM BINOMIAL \n")
  n=as.numeric(readline("Masukkan ukuran (n) : "))
  while (n<=0){
    cat("------tetot tetot-------\n")
    cat("ukurannya <0 \n")
    n=as.numeric(readline("Masukkan ukuran (n) : "))
    }
  p=as.numeric(readline("Masukkan nilai probabilitas : "))
  while((p>1)||(p<0)){
    cat("------tetot tetot-------\n")
    cat("nilai harus 0<p<1 \n")
    p=as.numeric(readline("Masukkan nilai probabilitas : "))
    }
    x=as.numeric(readline("Masukkan nilai yang di cari : "))
    while((x>n)||(x<0)){
      cat("------tetot tetot-------\n")
      cat("nilai harus 0<x<n \n")
      x=as.numeric(readline("Masukkan nilai yang di cari : "))
    }
  cat("\n Nilai probabilitas untuk",x,"sebesar",dbinom(x,n,p),"\n")
}
NO6()

#NO7
NO7<- function()
{
  e<- 0.00001
  delta<- 1
  xa<- scan(n-1)
  repeat {
    xb<- 1+exp(-2*xa)
    delta<- abs(xb-xa)
    if(delta< e) {
      break
    } else {
      xa<- xb
    }
  cat("f(x) minimum terjadi saat x= ",xb)
  }
}
NO7()

#NO8
NO8<- function()
{
  for(i in 1:40){
    cat("+-")
  }
  cat("\n")
  cat("NEWTON RAPHSON \n")
  e =as.numeric(readline("Masukkan batasan toleransi : "))
  x =as.numeric(readline("Masukkan nilai x awal : "))
  fxa<- ((-2*exp(-2*x))+(2*x)-2)
  fxb<- ((4+(2*exp(2*x)))/exp(2*x))
  err<- abs(fxa)
  while(err>e){
    x =x-(fxa/fxb)
    fxa<-((-2*exp(-2*x))+(2*x)-2)
    fxb<-((4+(2*exp(2*x)))/exp(2*x))
    err<-abs(fxa)
  }
  cat("Nilai x yang memenuhi persamaan tersebut adalah",x,"\n")
}
NO8()