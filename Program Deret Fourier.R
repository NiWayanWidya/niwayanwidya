#Latihan Deret Fourier
#Untuk Menghitung Nilai GCV Deret Fourier


GCV = function(data_fourier)
{
  t = as.numeric(data_fourier$t) #Menyatakan nilai X (Kebetulan X nya periode)
  Y = as.numeric(data_fourier$y) #Menyatakan nilai y 
  n = length(Y)
  lambda_0 = as.numeric(readline("Masukan Nilai Lambda Awal : "))
  lambda_1 = as.numeric(readline("Masukan Nilai Lambda Akhir : "))
  n_lambda = lambda_1 - lambda_0 + 1
  GCV = matrix(0,n_lambda,2)
  GCV[,1] = c(lambda_0:lambda_1)
  for(k in lambda_0:lambda_1)
  {
    a = c(0,k)
    for(j in 1:k)
    {
      a[j]=0
      for(i in 1:n)
      {
        a[j]=a[j]+((2/n)*Y[i]*cos((2*pi*j*(i-1))/n))
      }
    }
    b=c(0,k)
    for(j in 1:k)
    {
      b[j]=0
      for(i in 1:n)
      {
        b[j]=b[j]+((2/n)*Y[i]*sin((2*pi*j*(i-1))/n))
      }
    }
    beta_0 = sum(Y)/n
    GCV_atas = 0
    for(i in 1:n)
    {
      smt = 0
      for(j in 1:k)
      {
        smt = smt + (a[j]*cos((2*pi*j*(i-1))/n)+b[j]*sin((2*pi*j*(i-1))/n))
      }
      mt = beta_0 + smt
      GCV_atas = GCV_atas + ((1/n)*(Y[i]-mt)^2)
    }
    GCV[k - lambda_0 + 1,2]=GCV_atas/(1-(2*k+1)/n)^2
  }
  cat("\n")
  cat(" Matriks GCV : \n")
  cat("\n")
  lowest = GCV[1,2]
  for(k in 1:n_lambda)
  {
    if(GCV[k,2]<lowest)
    {
      lowest = GCV[k,2]
    }
  }
  cat("=======================================================================\n")
  cat("      lambda        Nilai GCV     \n")
  cat("=======================================================================\n")
  cat("\n")
  for(k in 1:n_lambda)
  {
    cat("      ",format(GCV[k,1]),"     ",format(GCV[k,2]), "\n")
  }
  cat("\n")
  cat("=======================================================================\n")
  cat("\n")
  cat(" Lambda dengan nilai GCV terkecil adalah sebagai berikut :")
  cat("\n")
  for(k in 1:n_lambda)
  {
    if(GCV[k,2]==lowest)
    {
      cat(" Nilai Lambda =", format(GCV[k,1]), ",dan nilai GCV = ", format(GCV[k,2]),"\n")
    }
  }
  cat("\n")
  win.graph()
  plot(GCV[,1],GCV[,2],type = "l", xlim=c(min(GCV[,1]),max(GCV[,1])),ylim = c(min(GCV[,2]),max(GCV[,2])),
       ylab="GCV",xlab="Lambda")
}

#Untuk Mengestimasi Data
Estimasi = function(data_fourier)
{
  t = as.numeric(data_fourier$t) #Menyatakan nilai X (Kebetulan X nya periode)
  Y = as.numeric(data_fourier$y) #Menyatakan nilai y 
  n = length(Y)
  k = as.numeric(readline("Lambda yang Optimal : "))
  emt = rep(0,n)
  a=c(0,k)
  b=c(0,k)
  d=matrix(0,k,2)
  for(j in 1:k)
  {
    a[j] = 0
    b[j] = 0
    for(i in 1:n)
    {
      a[j]=a[j]+((2/n)*Y[i]*cos((2*pi*j*(i-1))/n))
      b[j]=b[j]+((2/n)*Y[i]*sin((2*pi*j*(i-1))/n))
    }
    d[j,1] = a[j]
    d[j,2] = b[j]
  }
  beta_0 = sum(Y)/n
  mt = rep(0,n)  
  error = rep(0,n)
  for(i in 1:n)
  {
    smt = 0
    for(j in 1:k)
    {      smt = smt + (a[j]*cos((2*pi*j*(i-1))/n)+b[j]*sin((2*pi*j*(i-1))/n))
    }
  mt[i] = beta_0 + smt
  }
  y_topi = matrix(mt,n,1)
  residual = (Y-y_topi)
  cat("Tabel Hasil Estimasi : \n")
  cat("[Obs] \t\t [X] \t\t [Y] \t\t [Fit] \t\t\t [Residual] \n")
  for(i in 1:n)
  {
    cat(i,"\t\t",t[i],"\t\t",Y[i],"\t\t",y_topi[i],"\t\t",residual[i],"\n")
  }
  MSE = sum((Y-y_topi[,1])^2)/n
  R_squared = 1 - sum((Y-y_topi[,1])^2)/sum((Y-mean(Y))^2)
  cat("Nilai dari MSE dan R^2 adalah sebagai berikut. \n")
  cat("MSE = ",MSE, "  Rsq = ",R_squared)
  win.graph()
  plot(t,Y,type = "p",ylab="Yst", xlab="t")
  lines(t,mt,type="l")
  win.graph()
  plot(t,Y,type="p",xlim=c(min(t),max(t)),ylim=c(min(Y),max(Y)),ylab="Yst",xlab="t")
  lines(t,mt,type="l")
}