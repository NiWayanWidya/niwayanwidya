#Nama : Ni Wayan Widya Septia Sari
#Nim  : 082011833050

#NO1
library(tcltk)
library(tcltk2)
no1<-tktoplevel()
tktitle(no1)<-"Tipe yang Anda Inginkan"
topmenu<-tkmenu(no1)
submenu<-tkmenu(no1)
subsubmenu<-tkmenu(no1)
tkconfigure(no1,menu=topmenu)
tipe1<-tkmenu(topmenu,tearoff=FALSE)
tkadd(tipe1,"command",label="Luas Bangunan",command=function() tkdestroy(no1))
tkadd(tipe1,"command",label="Fasilitas",command=function() tkdestroy(no1))
tkadd(tipe1,"command",label="Harga",command=function() tkdestroy(no1))
tipe2<-tkmenu(topmenu,tearoff=FALSE)
sm1<-tkmenu(submenu,tearoff=FALSE)
sm2<-tkmenu(subsubmenu,tearoff=FALSE)
tkadd(tipe2,"cascade",label="Luas Bangunan",menu=sm1)
tkadd(tipe2,"command",label="Fasilitas",command=function() tkdestroy(no1))
tkadd(tipe2,"command",label="Harga",command=function() tkdestroy(no1))
tkadd(sm1,"command",label="Tipe Rumah 36",command=function() tkdestroy(no1))
tkadd(sm1,"cascade",label="Tipe Rumah 45",menu=sm2)
tkadd(sm2,"command",label="Reguler",command=function() tkdestroy(no1))
tkadd(sm2,"command",label="Penawaran Terbatas",command=function() tkdestroy(no1))
tkadd(topmenu,"cascade",label="Tipe I",menu=tipe1)
tkadd(topmenu,"cascade",label="Tipe II",menu=tipe2)

#NO2
no2<-tktoplevel()
tktitle(no2)<-"Uji Z"
teks<-tkfont.create(family="times",size=22,weight="bold",slant="roman")
teks1<-tkfont.create(family="times",size=14,slant="roman")
tkgrid(tklabel(no2,text=" >>> UJI Z <<<",font=teks),sticky="we")
tkgrid(tklabel(no2,text=" "))
tkgrid(tklabel(no2,text="Nama : Ni Wayan Widya Septia Sari",font=teks1),sticky="w")
tkgrid(tklabel(no2,text="NIM  : 082011833050",font=teks1),sticky="w")
tkgrid(tklabel(no2,text=" "))
tkgrid(tklabel(no2,text="Prodi S1 Statistika",font=teks1),sticky="we")
tkgrid(tklabel(no2,text="Universitas Airlangga",font=teks1),sticky="we")
tkgrid(tklabel(no2,text="2022",font=teks1),sticky="we")
tkgrid(tklabel(no2,text=" "))
topmenu=tkmenu(no2)
tkconfigure(no2,menu=topmenu)

zkiri<-function()
{
  library(tcltk)
  jend1=tktoplevel()
  tktitle(jend1)="Uji Z : Input Data"
  
  teks1=tkfont.create(family="sans",weight="bold",size=13)
  teks2=tkfont.create(family="times",weight="bold",size=12)
  teks3=tkfont.create(family="courier",size=12)
  teks4=tkfont.create(family="courier",size=10)
  
  tkgrid(tklabel(jend1,text="            INPUT DATA UJI Z   ",font=teks1))
  
  miuu=tclVar("0")
  eb1=tkentry(jend1,width="7",textvariable=miuu,font=teks3)
  tkgrid(tklabel(jend1,text=" Miu nol                   :",font=teks3),eb1,sticky="w")
  
  sigm=tclVar("0")
  eb2=tkentry(jend1,width="7",textvariable=sigm,font=teks3)
  tkgrid(tklabel(jend1,text=" Simpangan Baku Diketauhi  : ",font=teks3),eb2,sticky="w")
  
  rb1<-tkradiobutton(jend1)
  rb2<-tkradiobutton(jend1)
  rb3<-tkradiobutton(jend1)
  rbValue<-tclVar("1")
  tkconfigure(rb1,variable=rbValue,value="1")
  tkconfigure(rb2,variable=rbValue,value="2")
  tkconfigure(rb3,variable=rbValue,value="3")
  tkgrid(tklabel(jend1,text=" Masukkan Nilai Alpha      :",font=teks3),sticky="w")
  tkgrid(tklabel(jend1,text="1%",font=teks3),rb1)
  tkgrid(tklabel(jend1,text="5%",font=teks3),rb2)
  tkgrid(tklabel(jend1,text="10%",font=teks3),rb3)
  
  fungsi_OK=function()
  {
    tabel=edit(data.frame())
    
    n=length(tabel$var1)
    r=sum(tabel$var1)/n
    s2=var(tabel$var1)
    s=sqrt(var(tabel$var1))
    
    analisis=function()
    {
      jend2=tktoplevel()
      tkwm.title(jend2, "   Uji Z : Hasil Analisis")
      tkgrid(tklabel(jend2,text="=== HASIL ANALISIS UJI Z ===",font=teks1),sticky="n")
      
      miu=as.numeric(tclvalue(miuu))
      sigma=as.numeric(tclvalue(sigm))
      library(tcltk)
      teks1=tkfont.create(family="sans",weight="bold",size=13)
      teks2=tkfont.create(family="times",weight="bold",size=12)
      teks3=tkfont.create(family="courier",size=12)
      rbVal=as.character(tclvalue(rbValue))
      
      if(rbVal=="1")
      {
        alfa=0.01
      }
      if(rbVal=="2")
      {
        alfa=0.05
      }
      if(rbVal=="3")
      {
        alfa=0.1
      }
      
      tkgrid(tklabel(jend2,text=" "))
      tkgrid(tklabel(jend2,text="Statistika Deskriptif",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" Ukuran Sampel           : ",round(n,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Rata-rata Sampel        : ",round(r,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Varians Sampel          : ",round(s2,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Simpangan Baku Sampel   : ",round(s,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Simpangan Baku Populasi : ",sigma),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Nilai Alpha             : ",alfa),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=" "))
      
      #Mencari Nilai Z hitung 
      zhit= (r-miu)/(sigma/sqrt(n))
      pvalue=pnorm(zhit)
      
      #Kesimpulan
      tkgrid(tklabel(jend2,text="Hipotesis",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" H0 : miu = ",miu),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" H1 : miu <",miu),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=" "))
      tkgrid(tklabel(jend2,text="Hasil Perhitungan",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" Z hitung   :",round(zhit,4)),font=teks3),sticky="w")
      ztabel=qnorm(1-alfa)
      tkgrid(tklabel(jend2,text=paste(" Z Tabel    :",round(ztabel,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" P-Value    :",round(pvalue,3)),font=teks3),sticky="w")
      
      if(pvalue<alfa)
      {tkgrid(tklabel(jend2,text=" Keputusan  : Tolak H0",font=teks3),sticky="w")
        tkgrid(tklabel(jend2,text=paste(" Kesimpulan : miu <",miu),font=teks3),sticky="w")}
      else
      {tkgrid(tklabel(jend2,text=" Keputusan  : Gagal Tolak H0",font=teks3),sticky="w")
        tkgrid(tklabel(jend2,text=paste(" Kesimpulan : miu =",miu),font=teks3),sticky="w")}
      tkgrid(tklabel(jend2,text=" ")) 
    }
    
    
    tombolnext=tkbutton(jend1,text="HASIL ANALISIS",font=teks4,command=analisis)
    tkgrid(tombolnext,padx=5,pady=5)
    tkgrid(tklabel(jend1,text=" "))
  } 
  
  
  tombolOK=tkbutton(jend1,text="MASUKKAN DATA",font=teks4,command=fungsi_OK,bg="dark khaki")
  tkgrid(tombolOK)
  tkgrid(tklabel(jend1,text=" "))
}

zkanan<-function()
{
  library(tcltk)
  jend1=tktoplevel()
  tktitle(jend1)="Uji Z : Input Data"
  
  teks1=tkfont.create(family="sans",weight="bold",size=13)
  teks2=tkfont.create(family="times",weight="bold",size=12)
  teks3=tkfont.create(family="courier",size=12)
  teks4=tkfont.create(family="courier",size=10)
  
  tkgrid(tklabel(jend1,text="            INPUT DATA UJI Z   ",font=teks1))
  
  miuu=tclVar("0")
  eb1=tkentry(jend1,width="7",textvariable=miuu,font=teks3)
  tkgrid(tklabel(jend1,text=" Miu nol                     :",font=teks3),eb1,sticky="w")
  
  sigm=tclVar("0")
  eb2=tkentry(jend1,width="7",textvariable=sigm,font=teks3)
  tkgrid(tklabel(jend1,text=" Simpangan Baku Diketauhi    : ",font=teks3),eb2,sticky="w")
  
  rb1<-tkradiobutton(jend1)
  rb2<-tkradiobutton(jend1)
  rb3<-tkradiobutton(jend1)
  rbValue<-tclVar("1")
  tkconfigure(rb1,variable=rbValue,value="1")
  tkconfigure(rb2,variable=rbValue,value="2")
  tkconfigure(rb3,variable=rbValue,value="3")
  tkgrid(tklabel(jend1,text=" Masukkan Nilai Alpha        :",font=teks3),sticky="w")
  tkgrid(tklabel(jend1,text="1%",font=teks3),rb1)
  tkgrid(tklabel(jend1,text="5%",font=teks3),rb2)
  tkgrid(tklabel(jend1,text="10%",font=teks3),rb3)
  
  fungsi_OK=function()
  {
    tabel=edit(data.frame())
    
    n=length(tabel$var1)
    r=sum(tabel$var1)/n
    s2=var(tabel$var1)
    s=sqrt(var(tabel$var1))
    
    analisis=function()
    {
      jend2=tktoplevel()
      tkwm.title(jend2, "   Uji Z : Hasil Analisis")
      tkgrid(tklabel(jend2,text="=== HASIL ANALISIS UJI Z ===",font=teks1),sticky="n")
      
      miu=as.numeric(tclvalue(miuu))
      sigma=as.numeric(tclvalue(sigm))
      library(tcltk)
      teks1=tkfont.create(family="sans",weight="bold",size=13)
      teks2=tkfont.create(family="times",weight="bold",size=12)
      teks3=tkfont.create(family="courier",size=12)
      rbVal=as.character(tclvalue(rbValue))
      
      if(rbVal=="1")
      {
        alfa=0.01
      }
      if(rbVal=="2")
      {
        alfa=0.05
      }
      if(rbVal=="3")
      {
        alfa=0.1
      }
      
      tkgrid(tklabel(jend2,text=" "))
      tkgrid(tklabel(jend2,text="Statistika Deskriptif",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" Ukuran Sampel           : ",round(n,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Rata-rata Sampel        : ",round(r,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Varians Sampel          : ",round(s2,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Simpangan Baku Sampel   : ",round(s,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Simpangan Baku Populasi : ",sigma),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Nilai Alpha             : ",alfa),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=" "))
      
      #Mencari Nilai Z Hitung 
      zhit= (r-miu)/(sigma/sqrt(n))
      pvalue=1-pnorm(zhit)
      
      #Kesimpulan
      tkgrid(tklabel(jend2,text="Hipotesis",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" H0 : miu = ",miu),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" H1 : miu >",miu),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=" "))
      tkgrid(tklabel(jend2,text="Hasil Perhitungan",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" Z hitung   :",round(zhit,4)),font=teks3),sticky="w")
      ztabel=qnorm(1-alfa)
      tkgrid(tklabel(jend2,text=paste(" Z Tabel    :",round(ztabel,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" P-Value    :",round(pvalue,3)),font=teks3),sticky="w")
      
      if(pvalue<alfa)
      {tkgrid(tklabel(jend2,text=" Keputusan  : Tolak H0",font=teks3),sticky="w")
        tkgrid(tklabel(jend2,text=paste(" Kesimpulan : miu >",miu),font=teks3),sticky="w")}
      else
      {tkgrid(tklabel(jend2,text=" Keputusan  : Gagal Tolak H0",font=teks3),sticky="w")
        tkgrid(tklabel(jend2,text=paste(" Kesimpulan : miu =",miu),font=teks3),sticky="w")}
      tkgrid(tklabel(jend2,text=" ")) 
    }
    
    
    tombolnext=tkbutton(jend1,text="HASIL ANALISIS",font=teks4,command=analisis)
    tkgrid(tombolnext,padx=5,pady=5)
    tkgrid(tklabel(jend1,text=" "))
  } 
  
  
  tombolOK=tkbutton(jend1,text="MASUKKAN DATA",font=teks4,command=fungsi_OK,bg="dark khaki")
  tkgrid(tombolOK)
  tkgrid(tklabel(jend1,text=" "))
}

zduasisi<-function()
{
  library(tcltk)
  jend1=tktoplevel()
  tktitle(jend1)="Uji Z : Input Data"
  
  teks1=tkfont.create(family="sans",weight="bold",size=13)
  teks2=tkfont.create(family="times",weight="bold",size=12)
  teks3=tkfont.create(family="courier",size=12)
  teks4=tkfont.create(family="courier",size=10)
  
  tkgrid(tklabel(jend1,text="            INPUT DATA UJI Z   ",font=teks1))
  
  miuu=tclVar("0")
  eb1=tkentry(jend1,width="7",textvariable=miuu,font=teks3)
  tkgrid(tklabel(jend1,text=" Miu nol                   :",font=teks3),eb1,sticky="w")
  
  sigm=tclVar("0")
  eb2=tkentry(jend1,width="7",textvariable=sigm,font=teks3)
  tkgrid(tklabel(jend1,text=" Simpangan Baku Diketauhi  : ",font=teks3),eb2,sticky="w")
  
  rb1<-tkradiobutton(jend1)
  rb2<-tkradiobutton(jend1)
  rb3<-tkradiobutton(jend1)
  rbValue<-tclVar("1")
  tkconfigure(rb1,variable=rbValue,value="1")
  tkconfigure(rb2,variable=rbValue,value="2")
  tkconfigure(rb3,variable=rbValue,value="3")
  tkgrid(tklabel(jend1,text=" Masukkan Nilai Alpha      :",font=teks3),sticky="w")
  tkgrid(tklabel(jend1,text="1%",font=teks3),rb1)
  tkgrid(tklabel(jend1,text="5%",font=teks3),rb2)
  tkgrid(tklabel(jend1,text="10%",font=teks3),rb3)
  
  fungsi_OK=function()
  {
    tabel=edit(data.frame())
    
    n=length(tabel$var1)
    r=sum(tabel$var1)/n
    s2=var(tabel$var1)
    s=sqrt(var(tabel$var1))
    
    analisis=function()
    {
      jend2=tktoplevel()
      tkwm.title(jend2, "   Uji Z : Hasil Analisis")
      tkgrid(tklabel(jend2,text="=== HASIL ANALISIS UJI Z ===",font=teks1),sticky="n")
      
      miu=as.numeric(tclvalue(miuu))
      sigma=as.numeric(tclvalue(sigm))
      library(tcltk)
      teks1=tkfont.create(family="sans",weight="bold",size=13)
      teks2=tkfont.create(family="times",weight="bold",size=12)
      teks3=tkfont.create(family="courier",size=12)
      rbVal=as.character(tclvalue(rbValue))
      
      if(rbVal=="1")
      {
        alfa=0.01
      }
      if(rbVal=="2")
      {
        alfa=0.05
      }
      if(rbVal=="3")
      {
        alfa=0.1
      }
      
      tkgrid(tklabel(jend2,text=" "))
      tkgrid(tklabel(jend2,text="Statistika Deskriptif",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" Ukuran Sampel           : ",round(n,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Rata-rata Sampel        : ",round(r,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Varians Sampel          : ",round(s2,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Simpangan Baku Sampel   : ",round(s,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Simpangan Baku Populasi : ",sigma),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Nilai Alpha             : ",alfa),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=" "))
      
      #Mencari Z hitung 
      zhit= (r-miu)/(sigma/sqrt(n))
      pvalue=2*(1-pnorm(zhit))
      
      #Kesimpulan
      tkgrid(tklabel(jend2,text="Hipotesis",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" H0 : miu = ",miu),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" H1 : miu !=",miu),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=" "))
      tkgrid(tklabel(jend2,text="Hasil Perhitungan",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" Z hitung   :",round(zhit,4)),font=teks3),sticky="w")
      ztabel=qnorm(1-(alfa/2))
      tkgrid(tklabel(jend2,text=paste(" Z Tabel    :",round(ztabel,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" P-Value    :",round(pvalue,3)),font=teks3),sticky="w")
      
      if(pvalue<alfa)
      {tkgrid(tklabel(jend2,text=" Keputusan  : Tolak H0",font=teks3),sticky="w")
        tkgrid(tklabel(jend2,text=paste(" Kesimpulan : miu != ",miu),font=teks3),sticky="w")}
      else
      {tkgrid(tklabel(jend2,text=" Keputusan  : Gagal Tolak H0",font=teks3),sticky="w")
        tkgrid(tklabel(jend2,text=paste(" Kesimpulan : miu =",miu),font=teks3),sticky="w")}
      tkgrid(tklabel(jend2,text=" ")) 
    }
    
    
    tombolnext=tkbutton(jend1,text="HASIL ANALISIS",font=teks4,command=analisis)
    tkgrid(tombolnext,padx=5,pady=5)
    tkgrid(tklabel(jend1,text=" "))
  } 
  
  
  tombolOK=tkbutton(jend1,text="MASUKKAN DATA",font=teks4,command=fungsi_OK,bg="dark khaki")
  tkgrid(tombolOK)
  tkgrid(tklabel(jend1,text=" "))
}

menu1=tkmenu(topmenu,tearoff=FALSE)

tkadd(topmenu,"cascade",label="Uji Z",menu=menu1)

tkadd(menu1,"cascade",label="Uji Z Kiri",command=zkiri)
tkadd(menu1,"cascade",label="Uji Z Kanan",command=zkanan)
tkadd(menu1,"cascade",label="Uji Z Dua Sisi",command=zduasisi)



#NO3
no3<-tktoplevel()
tktitle(no3)<-"Uji t"
teks<-tkfont.create(family="times",size=22,weight="bold",slant="roman")
teks1<-tkfont.create(family="times",size=14,slant="roman")
tkgrid(tklabel(no3,text=" >>> UJI T <<<",font=teks),sticky="we")
tkgrid(tklabel(no3,text=" "))
tkgrid(tklabel(no3,text="Nama : Ni Wayan Widya Septia Sari",font=teks1),sticky="w")
tkgrid(tklabel(no3,text="NIM  : 082011833050",font=teks1),sticky="w")
tkgrid(tklabel(no3,text=" "))
tkgrid(tklabel(no3,text="Prodi S1 Statistika",font=teks1),sticky="we")
tkgrid(tklabel(no3,text="Universitas Airlangga",font=teks1),sticky="we")
tkgrid(tklabel(no2,text="2022",font=teks1),sticky="we")
tkgrid(tklabel(no3,text=" "))
topmenu=tkmenu(no3)
tkconfigure(no3,menu=topmenu)

info=function()
{
  tkmessageBox(message="Program ini belum bisa dijalankan. Coba pilih program lain",icon="info")
}

tkiri<-function()
{require(tcltk)
  jend1=tktoplevel()
  tktitle(jend1)<-"Uji t : Import Data"
  teks1=tkfont.create(family="sans",weight="bold",size=13)
  teks2=tkfont.create(family="times",weight="bold",size=12)
  teks3=tkfont.create(family="courier",size=12)
  teks4=tkfont.create(family="courier",size=10)
  teks5=tkfont.create(family="times",size=10,slant="italic")
  
  tkgrid(tklabel(jend1,text="Masukkan Data Excel",font=teks2))
  tkgrid(tklabel(jend1,text="Format file harus dalam bentuk .xlsx",font=teks5))
  tkgrid(tklabel(jend1,text="Data pada kolom pertama dan baris pertama diberi nama variabel",font=teks3))
  
  fungsitabel1=function()
  {
    tkdestroy(jend1)
    require(tcltk)
    jend2=tktoplevel()
    tktitle(jend2)<-"Uji t : Input Data"
    teks1=tkfont.create(family="sans",weight="bold",size=13)
    teks2=tkfont.create(family="times",weight="bold",size=12)
    teks3=tkfont.create(family="courier",size=12)
    teks4=tkfont.create(family="courier",size=10)
    tkgrid(tklabel(jend2,text="            UJI T   ",font=teks1))
    tkgrid(tklabel(jend2,text=" Data yang digunakan :",font=teks3),sticky="w")
    
    library(readxl)
    tclRequire("Tktable")
    upload=read_excel(file.choose(),col_names = TRUE)
    M=as.matrix(upload)
    
    
    TableView=function(dsn,dig=2)
    { 
      require(tcltk) 
      tclarray1=tclArray() 
      for (i in 0:(dim(dsn)[1])) { 
        for (j in 0:(dim(dsn)[2]-1)) { 
          if (i==0) { 
            tclarray1[[i,j]]=colnames(dsn)[j+1] 
          } else { 
            tem=dsn[i,j+1] 
            tclarray1[[i,j]]=ifelse(is.na(tem),".", 
                                    ifelse(is.numeric(tem),round(tem,digits=dig), 
                                           as.character(tem))) 
          } 
        } 
      } 
      return (tclarray1) 
    }
    
    temptable=TableView(upload)
    table1=tkwidget(jend2,"table",variable=temptable,rows=dim(upload)[1]+1,
                    cols=dim(upload)[2],titlerows=1,selectmode="extended",colwidth=12)
    tkgrid(table1,pady=20,padx=30)
    
    miuu=tclVar("0")
    eb1=tkentry(jend2,width="7",textvariable=miuu,font=teks3)
    tkgrid(tklabel(jend2,text=" Masukkan miu nol : ",font=teks3),eb1,sticky="w")
    
    rb1<-tkradiobutton(jend2)
    rb2<-tkradiobutton(jend2)
    rb3<-tkradiobutton(jend2)
    rbValue<-tclVar("1")
    tkconfigure(rb1,variable=rbValue,value="1")
    tkconfigure(rb2,variable=rbValue,value="2")
    tkconfigure(rb3,variable=rbValue,value="3")
    tkgrid(tklabel(jend2,text=" Masukkan Nilai Alpha",font=teks3),sticky="w")
    tkgrid(tklabel(jend2,text="1%",font=teks3),rb1)
    tkgrid(tklabel(jend2,text="5%",font=teks3),rb2)
    tkgrid(tklabel(jend2,text="10%",font=teks3),rb3)
    
    tkgrid(tklabel(jend2,text=" "))
    
    
    fungsi_OK=function()
    {
      x=as.numeric(M[,1])
      miu=as.numeric(tclvalue(miuu))
      n=length(x)
      r=sum(x)/n
      s2=var(x)
      s=sqrt(var(x))
      rbVal=as.character(tclvalue(rbValue))
      
      if(rbVal=="1")
      {
        alfa=0.01
      }
      if(rbVal=="2")
      {
        alfa=0.05
      }
      if(rbVal=="3")
      {
        alfa=0.1
      }
      
      require(tcltk)
      jend2=tktoplevel()
      tkwm.title(jend2, "   Uji t : Hasil Analisis")
      tkgrid(tklabel(jend2,text=" "))
      
      tkgrid(tklabel(jend2,text="=== HASIL ANALISIS UJI T ===",font=teks1),sticky="n")
      
      thit=(r-miu)/(s/sqrt(n))
      pvalue=pt(thit,(n-1))
      
      tkgrid(tklabel(jend2,text=" "))
      tkgrid(tklabel(jend2,text="Statistika Deskriptif",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" Ukuran Sampel         : ",round(n,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Rata-rata Sampel      : ",round(r,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Varians Sampel        : ",round(s2,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Simpangan Baku Sampel : ",round(s,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=" "))
      
      tkgrid(tklabel(jend2,text=" Hipotesis",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" H0 : miu = ",miu),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" H1 : miu <",miu),font=teks3),sticky="w")
      
      tkgrid(tklabel(jend2,text=" "))
      tkgrid(tklabel(jend2,text=" Hasil Analisis",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" T hitung  :",round(thit,3)),font=teks3),sticky="w")
      ttabel=qt(1-alfa,n-1)
      tkgrid(tklabel(jend2,text=paste(" T tabel   :",round(ttabel,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" P-Value   :",round(pvalue,3)),font=teks3),sticky="w")
      
      if(pvalue<alfa)
      {
        tkgrid(tklabel(jend2,text=" Keputusan : Tolak H0",font=teks3),sticky="w")
        tkgrid(tklabel(jend2,text=paste(" Kesimpulan : miu <",miu),font=teks3),sticky="w")
      }
      
      else
      {
        tkgrid(tklabel(jend2,text=" Keputusan : Gagal tolak H0",font=teks3),sticky="w")
        tkgrid(tklabel(jend2,text=paste(" Kesimpulan : miu =",miu),font=teks3),sticky="w")
      }
      tkgrid(tklabel(jend2,text=" "))
    }
    
    
    tombolOK=tkbutton(jend2,text="HASIL ANALISIS",font=teks4,command=fungsi_OK,bg="dark khaki")
    tkgrid(tombolOK)
    
    selesai=tkbutton(jend2,text="KELUAR",font=teks4,command=function()tkdestroy(jend4))
    tkgrid(selesai,padx=5,pady=5,sticky="n")
    tkgrid(tklabel(jend1,text=" "))
    
  } 
  
  tombolnext=tkbutton(jend1,text="MASUKKAN DATA",font=teks4,command=fungsitabel1,bg="dark khaki")
  tkgrid(tombolnext,padx=5,pady=5)
  
  selesai=tkbutton(jend1,text="KELUAR",font=teks4,command=function()tkdestroy(jend3))
  tkgrid(selesai,padx=5,pady=5,sticky="n")
  tkgrid(tklabel(jend1,text=" "))}

tkanan<-function()
{
  library(tcltk)
  jend1=tktoplevel()
  tktitle(jend1)<-"Uji t : Import Data"
  teks1=tkfont.create(family="sans",weight="bold",size=13)
  teks2=tkfont.create(family="times",weight="bold",size=12)
  teks3=tkfont.create(family="courier",size=12)
  teks4=tkfont.create(family="courier",size=10)
  teks5=tkfont.create(family="times",size=10,slant="italic")
  
  tkgrid(tklabel(jend1,text="Masukkan Data Excel",font=teks2))
  tkgrid(tklabel(jend1,text="Format file harus dalam bentuk .xlsx",font=teks5))
  tkgrid(tklabel(jend1,text="Data pada kolom pertama dan baris pertama diberi nama variabel",font=teks3))
  fungsitabel1=function()
  {
    tkdestroy(jend1)
    require(tcltk)
    jend2=tktoplevel()
    tktitle(jend2)<-"Uji t : Input Data"
    teks1=tkfont.create(family="sans",weight="bold",size=13)
    teks2=tkfont.create(family="times",weight="bold",size=12)
    teks3=tkfont.create(family="courier",size=12)
    teks4=tkfont.create(family="courier",size=10)
    tkgrid(tklabel(jend2,text="            UJI T   ",font=teks1))
    tkgrid(tklabel(jend2,text=" Data yang digunakan :",font=teks3),sticky="w")
    
    library(readxl)
    tclRequire("Tktable")
    upload=read_excel(file.choose(),col_names = TRUE)
    M=as.matrix(upload)
    
    
    TableView=function(dsn,dig=2)
    { 
      require(tcltk) 
      tclarray1=tclArray() 
      for (i in 0:(dim(dsn)[1])) { 
        for (j in 0:(dim(dsn)[2]-1)) { 
          if (i==0) { 
            tclarray1[[i,j]]=colnames(dsn)[j+1] 
          } else { 
            tem=dsn[i,j+1] 
            tclarray1[[i,j]]=ifelse(is.na(tem),".", 
                                    ifelse(is.numeric(tem),round(tem,digits=dig), 
                                           as.character(tem))) 
          } 
        } 
      } 
      return (tclarray1) 
    }
    
    temptable=TableView(upload)
    table1=tkwidget(jend2,"table",variable=temptable,rows=dim(upload)[1]+1,
                    cols=dim(upload)[2],titlerows=1,selectmode="extended",colwidth=12)
    tkgrid(table1,pady=20,padx=30)
    
    miuu=tclVar("0")
    eb1=tkentry(jend2,width="7",textvariable=miuu,font=teks3)
    tkgrid(tklabel(jend2,text=" Miu nol : ",font=teks3),eb1,sticky="w")
    
    rb1<-tkradiobutton(jend2)
    rb2<-tkradiobutton(jend2)
    rb3<-tkradiobutton(jend2)
    rbValue<-tclVar("1")
    tkconfigure(rb1,variable=rbValue,value="1")
    tkconfigure(rb2,variable=rbValue,value="2")
    tkconfigure(rb3,variable=rbValue,value="3")
    tkgrid(tklabel(jend2,text=" Masukkan Nilai Alpha",font=teks3),sticky="w")
    tkgrid(tklabel(jend2,text="1%",font=teks3),rb1)
    tkgrid(tklabel(jend2,text="5%",font=teks3),rb2)
    tkgrid(tklabel(jend2,text="10%",font=teks3),rb3)
    
    tkgrid(tklabel(jend2,text=" "))
    
    
    fungsi_OK=function()
    {
      x=as.numeric(M[,1])
      miu=as.numeric(tclvalue(miuu))
      n=length(x)
      r=sum(x)/n
      s2=var(x)
      s=sqrt(var(x))
      rbVal=as.character(tclvalue(rbValue))
      
      if(rbVal=="1")
      {
        alfa=0.01
      }
      if(rbVal=="2")
      {
        alfa=0.05
      }
      if(rbVal=="3")
      {
        alfa=0.1
      }
      
      require(tcltk)
      jend2=tktoplevel()
      tkwm.title(jend2, "   Uji t : Hasil Analisis")
      tkgrid(tklabel(jend2,text=" "))
      
      tkgrid(tklabel(jend2,text="=== HASIL ANALISIS UJI T ===",font=teks1),sticky="n")
      
      thit=(r-miu)/(s/sqrt(n))
      pvalue=1-pt(thit,(n-1))
      
      tkgrid(tklabel(jend2,text=" "))
      tkgrid(tklabel(jend2,text="Statistika Deskriptif",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" Ukuran Sampel         : ",round(n,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Rata-rata Sampel      : ",round(r,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Varians Sampel        : ",round(s2,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Simpangan Baku Sampel : ",round(s,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=" "))
      
      tkgrid(tklabel(jend2,text=" Hipotesis",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" H0 : miu = ",miu),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" H1 : miu >",miu),font=teks3),sticky="w")
      
      tkgrid(tklabel(jend2,text=" "))
      tkgrid(tklabel(jend2,text=" Hasil Analisis",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" T hitung  :",round(thit,3)),font=teks3),sticky="w")
      ttabel=qt(1-alfa,n-1)
      tkgrid(tklabel(jend2,text=paste(" T tabel   :",round(ttabel,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" P-Value   :",round(pvalue,3)),font=teks3),sticky="w")
      
      if(pvalue<alfa)
      {
        tkgrid(tklabel(jend2,text=" Keputusan : Tolak H0",font=teks3),sticky="w")
        tkgrid(tklabel(jend2,text=paste(" Kesimpulan : miu >",miu),font=teks3),sticky="w")
      }
      
      else
      {
        tkgrid(tklabel(jend2,text=" Keputusan : Gagal tolak H0",font=teks3),sticky="w")
        tkgrid(tklabel(jend2,text=paste(" Kesimpulan : miu =",miu),font=teks3),sticky="w")
      }
      tkgrid(tklabel(jend2,text=" "))
    }
    
    
    tombolOK=tkbutton(jend2,text="HASIL ANALISIS",font=teks4,command=fungsi_OK,bg="dark khaki")
    tkgrid(tombolOK)
    
    selesai=tkbutton(jend2,text="KELUAR",font=teks4,command=function()tkdestroy(jend4))
    tkgrid(selesai,padx=5,pady=5,sticky="n")
    tkgrid(tklabel(jend1,text=" "))
    
  } 
  
  tombolnext=tkbutton(jend1,text="MASUKKAN DATA",font=teks4,command=fungsitabel1,bg="dark khaki")
  tkgrid(tombolnext,padx=5,pady=5)
  
  selesai=tkbutton(jend1,text="KELUAR",font=teks4,command=function()tkdestroy(jend3))
  tkgrid(selesai,padx=5,pady=5,sticky="n")
  tkgrid(tklabel(jend1,text=" "))
}

tduasisi=function()
{
  library(tcltk)
  jend1=tktoplevel()
  tktitle(jend1)<-"Uji t : Import Data"
  teks1=tkfont.create(family="sans",weight="bold",size=13)
  teks2=tkfont.create(family="times",weight="bold",size=12)
  teks3=tkfont.create(family="courier",size=12)
  teks4=tkfont.create(family="courier",size=10)
  teks5=tkfont.create(family="times",size=10,slant="italic")
  
  tkgrid(tklabel(jend1,text="Masukkan Data Excel",font=teks2))
  tkgrid(tklabel(jend1,text="Format file harus dalam bentuk .xlsx",font=teks5))
  tkgrid(tklabel(jend1,text="Data pada kolom pertama dan baris pertama diberi nama variabel",font=teks3))
  fungsitabel1=function()
  {
    tkdestroy(jend1)
    require(tcltk)
    jend2=tktoplevel()
    tktitle(jend2)<-"Uji t : Input Data"
    teks1=tkfont.create(family="sans",weight="bold",size=13)
    teks2=tkfont.create(family="times",weight="bold",size=12)
    teks3=tkfont.create(family="courier",size=12)
    teks4=tkfont.create(family="courier",size=10)
    tkgrid(tklabel(jend2,text="            UJI T   ",font=teks1))
    tkgrid(tklabel(jend2,text=" Data yang digunakan :",font=teks3),sticky="w")
    
    library(readxl)
    tclRequire("Tktable")
    upload=read_excel(file.choose(),col_names = TRUE)
    M=as.matrix(upload)
    
    
    TableView=function(dsn,dig=2)
    { 
      require(tcltk) 
      tclarray1=tclArray() 
      for (i in 0:(dim(dsn)[1])) { 
        for (j in 0:(dim(dsn)[2]-1)) { 
          if (i==0) { 
            tclarray1[[i,j]]=colnames(dsn)[j+1] 
          } else { 
            tem=dsn[i,j+1] 
            tclarray1[[i,j]]=ifelse(is.na(tem),".", 
                                    ifelse(is.numeric(tem),round(tem,digits=dig), 
                                           as.character(tem))) 
          } 
        } 
      } 
      return (tclarray1) 
    }
    
    temptable=TableView(upload)
    table1=tkwidget(jend2,"table",variable=temptable,rows=dim(upload)[1]+1,
                    cols=dim(upload)[2],titlerows=1,selectmode="extended",colwidth=12)
    tkgrid(table1,pady=20,padx=30)
    
    miuu=tclVar("0")
    eb1=tkentry(jend2,width="7",textvariable=miuu,font=teks3)
    tkgrid(tklabel(jend2,text=" Miu nol : ",font=teks3),eb1,sticky="w")
    
    rb1<-tkradiobutton(jend2)
    rb2<-tkradiobutton(jend2)
    rb3<-tkradiobutton(jend2)
    rbValue<-tclVar("1")
    tkconfigure(rb1,variable=rbValue,value="1")
    tkconfigure(rb2,variable=rbValue,value="2")
    tkconfigure(rb3,variable=rbValue,value="3")
    tkgrid(tklabel(jend2,text=" Masukkan Nilai Alpha",font=teks3),sticky="w")
    tkgrid(tklabel(jend2,text="1%",font=teks3),rb1)
    tkgrid(tklabel(jend2,text="5%",font=teks3),rb2)
    tkgrid(tklabel(jend2,text="10%",font=teks3),rb3)
    
    tkgrid(tklabel(jend2,text=" "))
    
    fungsi_OK=function()
    {
      x=as.numeric(M[,1])
      miu=as.numeric(tclvalue(miuu))
      n=length(x)
      r=sum(x)/n
      s2=var(x)
      s=sqrt(var(x))
      rbVal=as.character(tclvalue(rbValue))
      
      if(rbVal=="1")
      {
        alfa=0.01
      }
      if(rbVal=="2")
      {
        alfa=0.05
      }
      if(rbVal=="3")
      {
        alfa=0.1
      }
      
      require(tcltk)
      jend2=tktoplevel()
      tkwm.title(jend2, "   Uji t : Hasil Analisis")
      tkgrid(tklabel(jend2,text=" "))
      
      tkgrid(tklabel(jend2,text="HASIL ANALISIS UJI T",font=teks1),sticky="n")
      
      thit=(r-miu)/(s/sqrt(n))
      pvalue=2*(1-pt(thit,(n-1)))
      
      tkgrid(tklabel(jend2,text=" "))
      tkgrid(tklabel(jend2,text="Statistika Deskriptif",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" Ukuran Sampel         : ",round(n,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Rata-rata Sampel      : ",round(r,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Varians Sampel        : ",round(s2,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" Simpangan Baku Sampel : ",round(s,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=" "))
      
      tkgrid(tklabel(jend2,text=" Hipotesis",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" H0 : miu = ",miu),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" H1 : miu !=",miu),font=teks3),sticky="w")
      
      tkgrid(tklabel(jend2,text=" "))
      tkgrid(tklabel(jend2,text=" Hasil Analisis",font=teks2))
      tkgrid(tklabel(jend2,text=paste(" T hitung :",round(thit,3)),font=teks3),sticky="w")
      ttabel=qt(1-alfa,n-1)
      tkgrid(tklabel(jend2,text=paste(" T tabel :",round(ttabel,3)),font=teks3),sticky="w")
      tkgrid(tklabel(jend2,text=paste(" P-Value :",round(pvalue,3)),font=teks3),sticky="w")
      
      if(pvalue<alfa)
      {
        tkgrid(tklabel(jend2,text=" Keputusan : Tolak H0",font=teks3),sticky="w")
        tkgrid(tklabel(jend2,text=paste(" Kesimpulan : miu =!",miu),font=teks3),sticky="w")
      }
      
      else
      {
        tkgrid(tklabel(jend2,text=" Keputusan : Gagal tolak H0",font=teks3),sticky="w")
        tkgrid(tklabel(jend2,text=paste(" Kesimpulan : miu =",miu),font=teks3),sticky="w")
      }
      tkgrid(tklabel(jend2,text=" "))
    }
    
    
    tombolOK=tkbutton(jend2,text="HASIL ANALISIS",font=teks4,command=fungsi_OK,bg="dark khaki")
    tkgrid(tombolOK)
    
    selesai=tkbutton(jend2,text="KELUAR",font=teks4,command=function()tkdestroy(jend4))
    tkgrid(selesai,padx=5,pady=5,sticky="n")
    tkgrid(tklabel(jend1,text=" "))
    
  } 
  
  tombolnext=tkbutton(jend1,text="MASUKKAN DATA",font=teks4,command=fungsitabel1,bg="dark khaki")
  tkgrid(tombolnext,padx=5,pady=5)
  
  selesai=tkbutton(jend1,text="KELUAR",font=teks4,command=function()tkdestroy(jend3))
  tkgrid(selesai,padx=5,pady=5,sticky="n")
  tkgrid(tklabel(jend1,text=" "))
  
}


menu1=tkmenu(topmenu,tearoff=FALSE)
menu2=tkmenu(topmenu,tearoff=FALSE)
sm1=tkmenu(menu1,tearoff=FALSE)
sm2=tkmenu(menu1,tearoff=FALSE)
sm3=tkmenu(menu1,tearoff=FALSE)

tkadd(topmenu,"cascade",label="Uji t",menu=menu1)

tkadd(menu1,"cascade",label="Uji t Kiri",menu=sm1)
tkadd(menu1,"cascade",label="Uji t Kanan",menu=sm2)
tkadd(menu1,"cascade",label="Uji t Dua Sisi",menu=sm3)

tkadd(sm1,"command",label="Data Ringkasan",command=info)
tkadd(sm1,"cascade",label="Input data",command=info)
tkadd(sm1,"cascade",label="Import data",command=tkiri)

tkadd(sm2,"command",label="Data Ringkasan",command=info)
tkadd(sm2,"cascade",label="Input data",command=info)
tkadd(sm2,"cascade",label="Import data",command=tkanan)

tkadd(sm3,"command",label="Data Ringkasan",command=info)
tkadd(sm3,"cascade",label="Input data",command=info)
tkadd(sm3,"cascade",label="Import data",command=tduasisi)



#NO4
jendela<-tktoplevel()
tktitle(jendela)<-"Plot Densitas Normal"
teks1<-tkfont.create(family="courier",size=28)
miu<-tclVar(" ")
sigma<-tclVar(" ")
n<-tclVar(" ")
tkgrid(tklabel(jendeka,text="PLOT DENSITAS NORMAL"))
eb1<-tkentry(jendela,width="50",textvariable=n)
tkgrid(tklabel(jendela,text="BANYAK DATA"),sticky="we")
tkgrid(eb1)
eb2<-tkentry(jendela,width="50",textvariable=miu)
tkgrid(tklabel(jendela,text="MEAN"),sticky="we")
tkgrid(eb2)
eb3<-tkentry(jendela,width="50",textvariable=sigma)
tkgrid(tklabel(jendela,text="STANDAR DEVIASI"),sticky="we")
tkgrid(eb3)
fungsiOK<-function()
{
  rata<-as.numeric(tclvalue(miu))
  s<-as.numeric(tclvalue(sigma))
  nn<-as.numeric(tclvalue(n))
  x<-sort(rnorm(nn,rata,s))
  y<-dnorm(x,rata,s)
  plot(x,y,type="l",col="red",lwd=4)
}
tombolOK<-tkbutton(jendela,text="OK",command=fungsiOK)
tkgrid(tombolOK)