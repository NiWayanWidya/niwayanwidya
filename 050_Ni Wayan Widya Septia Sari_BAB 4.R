#Tugas BAB 4
#NAMA : NI WAYAN WIDYA SEPTIA SARI 
#NIM  : 082011833050

#NO1
no1=function()
{
  cat("PROGRAM MENUKAR NILAI DUA VARIABLE")
  CAT("----------------------------------")
  x=readline("Input nilai X : ")
  y=readline("Input nilai y : ")
  z=x
  x=y
  y=z
  cat("\nHasil pertukaran nilai : \n")
  cat("Nilai x sekarang adalah : ",x, "\n")
  cat("Nilai y sekarang adalah : ",y, "\n")
}
no1()

#NO2
no2=function()
{
  r=as.numeric(readline("Input jari-jari : "))
  k<- 2*3.14*r
  l<- 3.14*(r^2)
  cat("Keliling lingkaran berjari - jari",r,"cm adalah:",keliling,"cm \n")
  cat("Luas lingkaran berjari - jari",r,"cm adalah:",luas,"cm \n")
}
no2()

#NO3
no3=function(x)
{
  cat("PROGRAM MENAMPILKAN HASIL FUNGSI y=ln(x)-exp(-x) \n")
  cat("------------------------------------------------ \n")
  cat("Input nilai x: ",x,"\n")
  cat("Hasilnya adalah ")
  y <- log(x)-exp(-x)
  y1 <- round(y,3)
  y2 <- ceiling(y)
  y3 <- floor(y)
  cat("Nilai y asli :",y,"\n")
  cat("Nilai y dibulatkan 3 digit :",y1,"\n")
  cat("Nilai y dibulatkan ke atas :",y2,"\n")
  cat("Nilai y dibulatkan ke bawah :",y3,"\n")
}
no3()

#NO4
no4+function()
{
  y <- as.numeric(readline("Input banyaknya vektor:"))
  cat("input nilai vektor:")
  x <- scan(n=y)
  jmlx <- sum(x)
  jmlxx <- sum(x^2)
  jmlerx <- sum((x-mean(x))^2)
  cat("Hasilnya adalah... \n")
  cat("Nilai jmlx :",jmlx,"\n")
  cat("Nilai jmlxx :",jmlxx,"\n")
  cat("Nilai jmlerx :",jmlerx,"\n")
}
no4()

#NO5
no5=function()
{
  cat("Input vektor x:")
  x <- scan()
  cat("Input vektor y:")
  y <- scan()
  jmlxy<-sum(x*y)
  cat("jmlxy = ", jmlxy, "\n")
  jlmerxy<-sum(x-mean(X))*(y_mean(y))
  cat("jmlerxy = ", jmlerxy, "\n")
}
no5()

#NO6
no6=function(a,b)
{
  cat("Input elemnt vektor x:")
  x <- scan(n=a)
  cat("Input element vektor y:")
  y <- scan(n=b)
  cat("vektor x = [", x,"]\n")
  cat("vektor y = [", y,"]\n")
  n <- a*(sum(x*y))-(sum(x)*sum(y))
  m <- (sqrt((a*sum(x^2))-(sum(x)^2)))*(sqrt((b*sum(y^2))-(sum(y)^2)))
  r <- n/m
  cat("Nilai korelasi dari vektor x dan y :",r,"\n")
}
no6()

#NO7
no7=function()
{
  n=as.numeric(readline("Masukkan ukuran (n) : "))
  p=as.numeric(readline("Masukkan nilai probabilitas : "))
  n<-seq(0,n,1)
  plot(x,dbinom(x,n,p),type = "1", main= 'distribusi binomial')
  cat("probalitas variable binomial pada suatu nilai x tertentu")
  cat("bila diketahui n=", n, "dan p=",p, "adalah \n")
  for(i in x)
  {
    cat("pada x",i, "adalah"pbinom(i,n,p),"\n")
  }
}
no7()

#NO8
no8=function
if(is.matrix(m))
{
  y<- as.vector(m[,1])
  x<- matrix(rep(1,nrow(m)),nrow=nrow(m),ncol=1)
  x<-cbind(x,m[,c(2:ncol(m))])
  x
  y
} else {
  print("m bukan merupakan matriks")
  }
}

#kalau matrix
m<- matrix(sq(1,12),4,3)
is.matrix(m)
no8()
x
y

#kalau bukan matrix
m<-matrix(sq(1,12))
a<-c(1:12)
is.matrix(m)
no8()
x
y

#NO9
#A
no9a=function(x)
{
  return(1+(8*x)-x^2)
}
no9a(seq(1,8))

#B
no9b=function(x)
{
  y=(1(8*x)-x^2)
  plot(x,y)
} 
no9b(seq(1,8))

#C
no9c=function()
{
  y=(1+(8*x)-x^2)
  plot(x,y,type="1")
}
9c(seq(1,8))

#D
no9d=function(x)
{
  y=(1+(8*x)-x^2)
  plot(x,y,type="b")
}
9d(seq(1,8))

#E
no9e=function(x)
{
  y=(1+(8*x)-x^2)
  plot(x,y)
  par(New=T)
  plot(x,y,type="1")
}
9e(seq(1,8))

#F
no9f=function(x)
{
  y=(1(8*x)-x^2)
  par(mfrow=c(2,2))
  plot(x,y)
  plot(x,y,type="1")
  plot(x,y,type="b")
  plot(x,y)
  par(New=T)
  plot(x,y,type="1")
}
no9f(seq(1,8))

#NO10
prakt2.10 <- function()
{
  x <- c(3.4,7.1,1.5,4.8,8.0,1.4,4.4,4.1,5.8,2.4)
  y <- c(24.04,18.49,16.25,25.16,13.00,15.64,25.24,25.09,23.56,20.84)
  X <- matrix(x,10,1)
  Y <- matrix(y,10,1)
  cat("Nilai matriks: \n")
  print(cbind(x,y))
  cat("Nilai x yang membuat y maksimum: \n")
  X[which.max(Y)]
}
prakt2.10()

plot2.10 <- function()
{
  x <- c(3.4,7.1,1.5,4.8,8.0,1.4,4.4,4.1,5.8,2.4)
  y <- c(24.04,18.49,16.25,25.16,13.00,15.64,25.24,25.09,23.56,20.84)
  X <- matrix(x,10,1)
  Y <- matrix(y,10,1)
  plot(x,y)
  plot(x,y,type="l",xlab="x",ylab="y")
}
plot2.10()

#NO11
prakt2.11 <- function(n)
{
  x <- rnorm(n,5,2)
  hist(x)
}
prakt2.11(10)

prakt2.11(30)
prakt2.11(60)
prakt2.11(90)
prakt2.11(150)
prakt2.11(300)

#NO12
no12=function()
{
  cat("input detik : ")
  s=scan(n-1)
  hari=trunc(s/86400)
  jam=trunc((s%86400)/3600)
  menit=trunc(((s%%86400)%%3600)/60)
  detik=((s%%86400))%%3600)%%60)
  cat("hasil perhitungan : \n")
  cat(s,"detik= ", hari, "hari= ", jam, "jam= ", menit, "menit= ", detik, "detik= ")
  
}
no12()



  