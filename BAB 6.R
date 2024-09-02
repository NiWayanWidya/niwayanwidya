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
NO4<- function()
{
  x<- 0
  y<- 1
  for(i in seq(from=1,to=11,by=0.1)){
    x[y]<-i
    y<- y+1
  }
  normal<- dnorm(x,mean=6,sd=2)
  plot(x,y,type-"1",
       lwd-2,
       main-"Dist.Normalnya",
       ylab-"PDF",
       )
}
NO4()

#NO5
NO5<- function() 
{
  #Dataframe
  (lower_bound <- seq(1, 91, 10))
  (upper_bound <- seq(10, 100, 10))
  freq
  (groupdata <- data.frame(lower_bound, upper_bound, freq))
  
  #Menghitung nilai kumulatif frekuensi
  cfreq <- NULL
  for(i in 1:nrow(groupdata)) {
    cfreq[i] <- 0
    for(j in 1:i) {
      cfreq[i] <- cfreq[i] + groupdata$freq[[j]]
    }
  }
  #Menggabungkan kumulatif frekuensi kedalam data frame
  (groupdata <- cbind(groupdata, cfreq))
  
  #Print Data
  print(groupdata)
  
  sumval <- 0
  for(i in 1:nrow(groupdata)) {
    sumval <- sumval + median(seq(groupdata$lower_bound[[i]], groupdata$upper_bound[[i]])) * groupdata$freq[[i]]
  }
  mean <- sumval / sum(groupdata$freq)
  cat("Mean data adalah", mean, "\n")
  
  sumvaroid <- 0
  for(i in 1:nrow(groupdata)) {
    sumvaroid <- sumvaroid + groupdata$freq[[i]] * (median(seq(groupdata$lower_bound[[i]], groupdata$upper_bound[[i]])) - mean)^2
  }
  var <- sumvaroid / (sum(groupdata$freq) - 1)
  cat("Variansi data adalah", var, "\n")
  
  # n/2
  (nlmed <- sum(groupdata$freq) / 2)
  groupdata
  fmed <- groupdata$cfreq[groupdata$cfreq > nlmed][1]
  medindx <- match(fmed, groupdata$cfreq)
  lmed <- groupdata$lower_bound[[medindx]] - 0.5
  c <- length(seq(groupdata$lower_bound[[medindx]], groupdata$upper_bound[[medindx]]))
  fmedsub1 <- groupdata$cfreq[[medindx - 1]]
  (median <- lmed + (c * (nlmed - fmedsub1)) / fmed)
  # Full Formula Median
  median_full <- groupdata$lower_bound[[medindx]] - 0.5 + ((length(seq(groupdata$lower_bound[[medindx]], groupdata$upper_bound[[medindx]])) * ((sum(groupdata$freq) / 2 ) - groupdata$cfreq[[medindx - 1]])) / groupdata$cfreq[groupdata$cfreq > nlmed][1])
  median_full
  cat("Median data adalah", median, "\n")
  
  # Modus
  groupdata
  (modusindx <- match(max(groupdata$freq), groupdata$freq))
  c <- length(seq(groupdata$lower_bound[[modusindx]], groupdata$upper_bound[[modusindx]]))
  a <- groupdata$freq[[modusindx]] - groupdata$freq[[modusindx-1]]
  b <- groupdata$freq[[modusindx]] - groupdata$freq[[modusindx+1]]
  lmo <- groupdata$lower_bound[[modusindx]] - 0.5
  modus <- lmo + c * (a / (a + b))
  cat("Modus data adalah", modus, "\n")
}
NO5()

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