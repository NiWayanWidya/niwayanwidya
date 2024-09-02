tugasm3=function()
{
  cat("Program Membangkitkan Angka\n")
  cat("Pilihan Metode :\n")
  cat("1. Metode Multiplicative Congrential RG\n")
  cat("2. Metode Mixed Congrential RG\n")
  metode=as.numeric(readline("Metode yang dipilih : "))
  
  if(metode==1)
  {
    seed=as.numeric(readline("Banyak seed yang diinginkan = "))
    s=rep(1:seed)
    for(i in 1:seed)
    {
      s[i]=as.numeric(readline("Input Seed (z) : "))
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
    seed=as.numeric(readline("Banyak seed yang diinginkan = "))
    s=rep(1:seed)
    for(i in 1:seed)
    {
      s[i]=as.numeric(readline("Seed (z0) : "))
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
  cat("\nMatriks x\n")
  print(x)
  cat("\nMatriks u\n")
  print(u)
}
tugasm3()