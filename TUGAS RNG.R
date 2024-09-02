TugasRNG=function()
{
  cat("=====================================TUGAS SIMULASI S3==================================\n")
  cat("NAMA : NI WAYAN WIDYA SEPTIA SARI\n")
  cat("NIM  : 082011833050\n")
  cat("========================================================================================\n")
  cat("Pilihan Metode Random Generated Number :\n")
  cat("1. Metode Multiplicative Congrential RNG\n")
  cat("2. Metode Mixed Congrential RNG\n")
  metode=as.numeric(readline("Metode yang dipilih : "))
  
  if(metode==1)
  {
    cat("========================================================================================\n")
    cat("Anda Memilih Metode Multiplicative Congrential RNG\n")
    seed=as.numeric(readline("Input banyak seed yang diinginkan = "))
    while(seed<1)
    {
      cat("MINIMAL BANYAK SEED ADALAH SATU !!")
      seed<-as.numeric(readline("Input banyak seed yang diinginkan = "))
    }
    s=rep(1:seed)
    for(i in 1:seed)
    {
      s[i]=as.numeric(readline("Input Seed : "))
    }
    a=as.numeric(readline("Nilai a = "))
    m=as.numeric(readline("Nilai m = "))
    n=as.numeric(readline("Banyak bilangan yang diinginkan = "))
    x=matrix(rep(1:n,seed), nrow=n, ncol=seed)
    u=matrix(rep(1:n,seed), nrow=n, ncol=seed)
    for(j in 1:seed)
    {
      for(i in 1:n)
      {
        if(i==1)
        {
          x[1,j]=(a*s[j])%%m
          u[1,j]=x[1,j]/m
        }
        else
        {
          x[i,j]=(a*x[i-1,j])%%m
          u[i,j]=x[i,j]/m
        } 
      }
    }
  }
  
  if(metode==2)
  {
    cat("========================================================================================\n")
    cat("Anda Memilih Mixed Congrential RNG\n")
    seed=as.numeric(readline("Input banyak seed yang diinginkan = "))
    while(seed<1)
    {
      cat("MINIMAL BANYAK SEED ADALAH SATU !!")
      seed<-as.numeric(readline("Input banyak seed yang diinginkan = "))
    }
    s=rep(1:seed)
    for(i in 1:seed)
    {
      s[i]=as.numeric(readline("Input Seed : "))
    }
    a=as.numeric(readline("Nilai a = "))
    m=as.numeric(readline("Nilai m = "))
    c=as.numeric(readline("Nilai c = "))
    n=as.numeric(readline("Banyak bilangan yang diinginkan = "))
    x=matrix(rep(1:n,seed), nrow=n, ncol=seed)
    u=matrix(rep(1:n,seed), nrow=n, ncol=seed)
    for(j in 1:seed)
    {
      for(i in 1:n)
      {
        if(i==1)
        {
          x[1,j]=(a*s[j]+c)%%m
          u[1,j]=x[1,j]/m
        }
        else
        {
          x[i,j]=(a*x[i-1,j]+c)%%m
          u[i,j]=x[i,j]/m
        } 
      }
    }
  }
  
  cat("\n")
  for(i in 1:seed)
  {
    cat("Angka bangkitan dari z[",i,"]=",s[i],"\n")
    print(cbind(x[,i],u[,i]))
    
  }
  cat("========================================================================================\n")
  cat("\nMatriks x : \n")
  print(x)
  
  cat("========================================================================================\n")
  cat("\nMatriks u : \n")
  print(u)
  }
TugasRNG()
