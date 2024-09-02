antrian=function()
{
  library("readxl")
  cat("===========================================\n")
  cat("         Program Antrian M/M/c \n")
  cat("===========================================\n")
  cat("  Kelompok 2 Simulasi || Kelas S2 || 2022\n")
  cat("-------------------------------------------\n")
  cat("Ayuning Dwis Cahyasari (01911833005)\n")
  cat("Helda Urbhani Rosa     (01911833016)\n")
  cat("Sasti Putri Salsabila  (01911833045)\n")
  cat("Sefanni Nur Ramadhani  (01911833056)\n")
  cat("Mokhamad In'am Hikami (01911833071)\n")
  cat("-------------------------------------------\n")
  cat("Dosen Pengampu Mata Kuliah Simulasi 2022\n")
  cat("         Dr. Nur Chamidah, M.Si\n")
  cat("       Dr. Toha Saifudin,S.Si,M.Si\n")
  cat("===========================================\n")
  
  #input data
  cat("\n----------------------------------\n")
  cat("       INPUT DATA PERMINTAAN\n")
  cat("----------------------------------\n")
  data=read_excel("KULIAH/SMT 6/Simulasi/M11 simulasi antrian MMC.xlsx")
  tanggal=ts(data$Tanggal)
  positif=ts(data$positif)
  sembuh=ts(data$sembuh)
  cat("Hasil analisis perhitungan: \n")
  n=as.numeric(readline("Jumlah hari             = "))
  k=as.numeric(readline("Jumlah server           = "))
  
  #laju kedatangan dan laju pelayanan
  lambda=sum(positif)/n #laju kedatangan
  miu=sum(sembuh)/n #laju pelayanana
  cat("Laju kedatangan          = ",lambda,"\n")
  cat("Laju pelayanan 3 server  = ",miu,"\n")
  
  #mencari ukuran steady state
  rho=lambda/miu
  cat("rho sebesar              = ",rho,"\n")
  if(rho<1) {cat("Server telah steady state\n")}
  else
  {
    cat("\nServer belum steady state, maka dilakukan pencarian penambahan server\n")
    miu1=miu/k
    cat("Laju pelayanan 1 serve   =",miu1,"\n")
    tambah=as.numeric(readline("\nJumlah tambahan maksimal server= "))
    c=k+tambah
    miu2=vector()
    rho1=vector()
    for(i in 1:tambah)
    {
      miu2[i]=(k+i)*miu1
      rho1[i]=lambda/miu2[i]
    }
    cat("===========================================\n")
    cat("    TABEL PENENTUAN PENAMBAHAN SERVER\n")
    cat("-------------------------------------------\n")
    cat("Server\t miu\t\t rho\n" )
    cat("-------------------------------------------\n")
    for(i in 1:tambah)
    {
      cat(k+i,"\t",miu2[i],"\t",rho1[i],"\n")
    }
    #perulangan untuk menghentikan server ketika sudah steady state
    j=1
    repeat
    {
      miu2[j]=(k+j)*miu1
      rho1[j]=lambda/miu2[j]
      if(rho1[j]<1)
      {
        cat("-------------------------------------------\n")
        cat("===========================================\n")
        cat("\nSehingga, dapat diambil keputusan:\n" )
        cat("Keadaan telah steady state dengan server sebanyak",k+j,"\n" )
        cat("Keadaan telah steady state dengan rho sebesar",rho1[j],"\n" )
        cat("Keadaan telah steady state, dengan server",k+j,"dengan miu sebesar",miu2[j],"\n" )
        break
      }
      j=j+1
    }
  }
  
  #mencari pengukuran kinerja sistem antrian
  cat("\n-----------------------------------------\n")
  cat("Mencari Kriteria Antrian\n")
  nn=as.numeric(readline("Jumlah bangkitan hari="))
  
  customer=vector()
  kedatangan=vector()
  mtrans=vector()
  keluar=vector()
  antri=vector()
  sistem=vector()
  
  interar=round(runif(nn,miu,lambda),3)
  trans=round(runif(nn,miu,lambda),3)
  kedatangan=round(kedatangan,3)
  mtrans=round(mtrans,3)
  keluar=round(keluar,3)
  antri=round(antri,3)
  sistem=round(sistem,3)
  
  
  customer[1]=1
  kedatangan[1]=interar[1]
  
  mtrans[1]=kedatangan[1]
  keluar[1]=trans[1]+mtrans[1]
  antri[1]=mtrans[1]-kedatangan[1]
  sistem[1]=antri[1]+trans[1]
  
  for(i in 2:nn)
  {
    customer[i]=i
    kedatangan[i]=interar[i]+kedatangan[i-1]
  }
  
  for(i in 2:nn)
  {
    if(kedatangan[i]>keluar[i-1]) 
    {mtrans[i]=kedatangan[i]}
    else if(kedatangan[i]<keluar[i-1]) 
    {mtrans[i]=keluar[i-1]}
    
    keluar[i]=mtrans[i]+trans[i]
    antri[i]=mtrans[i]-kedatangan[i]
    sistem[i]=antri[i]+trans[i]
  }
  
  cat("=============================================================================================================\n")
  cat("No\t Interr           Trans           Kedat           Mulai           Keluar           Antri           sistem\n")
  cat("=============================================================================================================\n")
  for(i in 2:(nn+1))
  {
    cat("",i-1,"\t",interar[i-1],"         ",trans[i-1],"        ",kedatangan[i-1],"          ",mtrans[i-1],"         ",keluar[i-1],"          ",antri[i-1],"         ",sistem[i-1],"\n")
  }
  cat("=============================================================================================================\n")
  
  ratawt=vector()
  rataser=vector()
  ratasis=vector()
  ratajwt=vector()
  ratajser=vector()
  ratajsis=vector()
  
  ratawt=round(ratawt,3)
  rataser=round(rataser,3)
  ratasis=round(ratasis,3)
  ratajwt=round(ratajwt,3)
  ratajser=round(ratajser,3)
  ratajsis=round(ratajsis,3)
  
  ratawt=sum(antri)/nn
  rataser=sum(trans)/nn
  ratasis=sum(sistem)/nn
  ratajwt=sum(antri)/max(keluar)
  ratajser=sum(trans)/max(keluar)
  ratajsis=sum(sistem)/max(keluar)
  
  cat("\n-----------------------------------------------------\n")
  cat("Rata-rata waktu tunggu                      =",ratawt,"\n")
  cat("Rata-rata servis                            =",rataser,"\n")
  cat("Rata-rata dalam sistem                      =",ratasis,"\n")
  cat("Rata-rata jumlah pasien dalam tempat tunggu =",ratajwt,"\n")
  cat("Rata-rata jumlah pasien dalam servis        =",ratajser,"\n")
  cat("Rata-rata jumlah pasien dalam sistem        =",ratajsis,"\n")
  