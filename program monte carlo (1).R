monte_carlo=function()
{
  cat("========================================================\n")
  cat("        Program Simulasi Prediksi dengan Monte Carlo \n")
  cat("========================================================\n")
  cat("  Kelompok 4 Simulasi || Kelas S3 || 2023\n")
  cat("-------------------------------------------\n")
  cat("Ni Wayan Widya Septia Sari (082011833050)\n")
  cat("Andini Putri Mediani	      (082011833067)\n")
  cat("Audrey Regina Cahyani      (082011833070)\n")
  cat("Ainaya Zakiyah Nabila      (082011833091)\n")
  cat("-------------------------------------------\n")
  cat("Dosen Pengampu Mata Kuliah Simulasi 2023\n")
  cat("         Elly Pusporani, S.Si., M.Stat\n")
  cat("       Sa'idah Zahrotul Jannah, S.Si.,M.Stat.\n")
  cat("========================================================\n\n")
  
  n=as.numeric(readline("Masukkan Jumlah Hari Prediksi\t= "))
  p=as.numeric(readline("Masukkan Banyak Perulangan   \t= "))
  x=rep(0,n)
  y=rep(0,n)
  k=rep(0,n)
  ratax=rep(0,p)
  ratay=rep(0,p)
  variansix=rep(0,p)
  
  cat("\n======================================================\n")
  cat("Nilai Prediksi Kedatangan untuk ",n," Hari Kedepan")
  cat("\n======================================================\n\n\n")
  
  for(j in 1:p)
  {
    cat("\nNilai Prediksi Kedatangan untuk ",n," Hari Kedepan Perulangan ke-",j,"\n")
    cat("-------------------------------------------------------------------------\n")
    cat("No\tBilangan Random\t\tx[i]\n")
    cat("-------------------------------------------------------------------------\n")
    for(i in 1:n)
    {
      R=runif(1)
      if(R<=0.29) x[i]=15
      else if(R<=0.36) x[i]=33
      else if(R<=0.47) x[i]=42
      else if(R<=0.72) x[i]=60
      else x[i]=86.5
      cat(" ",i,"\t",round(R,5),"\t\t",x[i],"\n")
    }
    cat("-------------------------------------\n")
    ratax[j]=mean(x)
    sumratax=ratax[j]+ratax[j-1]
    rataxx=sumratax/p
    cat("Mean Kedatangan ke-",j," =",ratax[j],"\n")
  }
  cat("\n==========================\n")
  cat("Mean Kedatangan =",rataxx,"\n")
  cat("==========================\n\n\n")
  
  cat("\n================================================\n")
  cat("Nilai Prediksi Pelayanan untuk ",n," Hari Kedepan")
  cat("\n================================================\n\n\n")
  for(j in 1:p)
  {
    cat("\nNilai Prediksi Pelayanan untuk ",n," Hari Kedepan Perulangan ke-",j,"\n")
    cat("-------------------------------------\n")
    cat("No\tBilangan Random\t\ty[i]\n")
    cat("-------------------------------------\n")
    for(i in 1:n)
    {
      R=runif(1)
      if(R<=0.30693) y[i]=16
      else if(R<=0.40594) y[i]=37
      else if(R<=0.64356) y[i]=55
      else if(R<=0.92079) y[i]=79.5
      else y[i]=96
      cat(" ",i,"\t",round(R,5),"\t\t",y[i],"\n")
    }
    cat("-------------------------------------\n")
    ratay[j]=mean(y)
    sumratay=ratay[j]+ratay[j-1]
    ratayy=sumratay/p
    cat("Mean Pelayanan ke-",j," =",ratay[j],"\n")
  }
  cat("\n=========================\n")
  cat("Mean Pelayanan =",ratayy,"\n")
  cat("=========================\n")
}
monte_carlo()