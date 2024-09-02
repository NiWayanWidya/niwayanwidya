#NAMA : NI WAYAN WIDYA SEPTIA SARI 
#NIM : 082011833050

#NO1
library(tcltk)
library(tcltk2)
NO1<-tktoplevel()
tkwm.title(NO1,"DATA MAHASISWA")
teks<-tkfont.create(family="courier", size=15, slant="roman")
tkgrid(tklabel(NO1, text="Nama Mahasiswa: Ni Wayan Widya", font=teks), sticky="w")
tkgrid(tklabel(NO1, text="NIM : 082011833050", font=teks),sticky="w")
tkgrid(tklabel(NO1, text="Prodi S1 Statistika", font=teks))
tkgrid(tklabel(NO1, text="Universitas Airlangga", font=teks))
tutup<-tkbutton(NO1, text="TUTUP", command=function()tkdesktroy(NO1))
tkgrid(tutup)

#NO2
NO2<- function()
{
  library(tcltk)
  library(tcltk2)
  jendela<-tktoplevel()
  tkwm.title(jendela,"Latihan No 2")
  tkgrid(tklabel(jendela,text="Nama :"),sticky="w")
  eb<-tkentry(jendela,width="50",textvariable=text)
  tkgrid(eb)
  tkgrid(tklabel(jendela,text="umur :"),sticky="w" )
  tspin<-tk2spinbox(jendela,from=0,to=99,increment=1)
  tkgrid(tspin)
  slidervalue<tclvar()
  slidervaluelabel<-tklabel(jendela,text=as.character(tclvalue(Slidervalue)))
  tkgrid(tklabel(jendela,text="kepuasan :"),slidervaluelabel,tklabel(jendela),sticky="w")
  tkconfigure(slidervaluelabel,textvariable=slidervalue)
  slider<-tkscale(jendela, from=0, to=100,showvalue=T,variable=slidervalue,orient="horizontal")
  tkgrid(tklabel(jendela,text=" "))
  tkgrid(slider)
  tutup<-tkbutton(jendela,text="TUTUP",command=function()tkdestroy(jendela))
  tkgrid(tutup)
}
NO2()
