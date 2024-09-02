GCV<-function(`PositifRateCovid.(1)`){
  t<- `PositifRateCovid.(1)`[,1]
  Y<-`PositifRateCovid.(1)`[,2]
  n<-length(Y)
  lawal<-as.numeric(readline("Masukkan lambda awal: "))
  lakhir<-as.numeric(readline("Masukkan lambda akhir: "))
  nlambda<-lakhir-lawal+1
  GCV<- matrix(0,nlambda,2)
  GCV[,1]<-c(lawal:lakhir)
  for(k in lawal:lakhir){
    a<-c(0,k)
    for(j in 1:k){
      a[j]<-0
      for(i in 1:n){
        a[j]<- a[j]+((2/n)*Y[i]*cos((2*pi*j*(i-1))/n))
      }
    }
    b<-c(0,k)
    for(j in 1:k){
      b[j]<-0
      for(i in 1:n){
        b[j]<-b[j]+((2/n)*Y[i]*sin((2*pi*j*(i-1))/n))
      }
    }
    betanol<-sum(Y)/n
    GCVatas<-0
    for(i in 1:n){
      smt<-0
      for(j in 1:k){
        smt<-smt+(a[j]*cos((2*pi*j*(i-1))/n)+b[j]*sin((2*pi*j*(i-1))/n))
      }
      mt<-betanol+smt
      GCVatas<-GCVatas+((1/n)*(Y[i]-mt)^2)
    }
    GCV[k-lawal+1, 2]<-GCVatas/(1-(2*k+1)/n)^2
  }
  cat("\n")
  cat(" Matriks GCV: \n")
  cat("\n")
  terkecil<-GCV[1,2]
  for(k in 1:nlambda){
    if(GCV[k,2]<terkecil){
      terkecil<-GCV[k,2]
    }
  }
  cat("========================================\n")
  cat("   lambda              nilai GCV  \n")
  cat("========================================\n")
  cat("\n")
  for(k in 1:nlambda){
    cat("    ", format(GCV[k,1]), "           ", format(GCV[k,2]), "\n")
  }
  cat("\n")
  cat("========================================\n")
  cat("\n")
  cat("Lambda dengan nilao GCV terkecil adalah sebagai berikut: ")
  cat("\n")
  for(k in 1:nlambda){
    if(GCV[k,2]==terkecil){
      cat("Nilai lambda= ", format(GCV[k,1]), ", dan nilai GCV= ", format(GCV[k,2]), "\n")
    }
  }
  cat("\n")
  win.graph()
  plot(GCV[,1], GCV[,2], type="l", xlim=c(min(GCV[,1]), max(GCV[,1])), ylim=c(min(GCV[,2]), max(GCV[,2])), ylab="GCV", xlab="Lambda")
}
GCV(`PositifRateCovid.(1)`)


estimasi<-function(`PositifRateCovid.(1)`){
  t<-`PositifRateCovid.(1)`[,1]
  Y<-`PositifRateCovid.(1)`[,2]
  n<-length(t)
  k<-as.numeric(readline("Lambda yang optimal: "))
  emt<-rep(0,n)
  a<-c(0,k)
  b<-c(0,k)
  d<-matrix(0,k,2)
  for(j in 1:k){
    a[j]<-0
    b[j]<-0
    for(i in 1:n){
      a[j]<- a[j]+((2/n)*Y[i]*cos((2*pi*j*(i-1))/n))
      b[j]<- b[j]+((2/n)*Y[i]*cos((2*pi*j*(i-1))/n))
    }
    d[j,1]<-a[j]
    d[j,2]<-b[j]
  }
  betanol<-sum(Y)/n
  mt<-rep(0,n)
  emt<-rep(0,n)
  error<-rep(0,n)
  for(i in 1:n){
    smt<-0
    for(j in 1:k){
      smt<-smt+(a[j]*cos((2*pi*j*(i-1))/n)+b[j]*sin((2*pi*j*(i-1))/n))
    }
    mt[i]<-betanol+smt
  }
  win.graph()
  plot(t, Y, type="p",ylab="Yst", xlab="t")
  lines(t,mt,type="l")
  win.graph()
  plot(t,Y,type="p",xlim=c(min(t), max(t)), ylim=c(min(Y), max(Y)), ylab="Y", xlab="t")
  lines(t,mt,type="l")
}
estimasi(`PositifRateCovid.(1)`)