MPL<-function(x,eps=1e-20)
{
  x<-as.matrix(x)
  xsvd<-svd(x)
  diago<-xsvd$d[xsvd$d>eps]
  if(length(diago)==1)
  {
    xplus<-as.matrix(xsvd$v[,1])%*%t(as.matrix(xsvd$u[,1])/diago)
  }
  else
  {
    xplus<-
      xsvd$v[,1:length(diago)]%*%diag(1/diago)%*%t(xsvd$u[,1:length(diago)])
  }
  return(xplus)
}
library(readxl)
DataImport=read_excel("chlorine.xlsx")
prediktor=DataImport$`Weeks`
respons=DataImport$`Chlorine`
data=cbind(prediktor,respons)
gcv1<-function(x)
{
	a<-as.numeric(readline("inputkan batas atas = "))
	b<-as.numeric(readline("inputkan batas bawah = "))
	inc<-as.numeric(readline("inputkan increment = "))
	m1<-as.numeric(readline("inputkan order = "))
	m<-m1+1
  k<-seq(b,a,inc)
  v<-length(k)
	x1<-data[,1]
	y1<-data[,2]
	x<-as.matrix(x1)
	y<-as.matrix(y1)
  n<-length(y)
  Gcv<-matrix(nrow=v,ncol=1)
  Mse<-matrix(nrow=v,ncol=1)
  for (j in 1:v)
  {
    w<-matrix(0,ncol=m+1,nrow=n)
    for (i in 1:m)
      w[,i]<-x^(i-1)
    for (i in m+1)
      w[,i]<-trun(x,k[j],m-1)
    wtw<- t(w) %*% w
    z<- MPL(wtw) 
    beta<- z %*% (t(w) %*% y)
    h<- w %*% z %*% t(w)
    mu<-w%*%beta
    MSE<- t(y-mu) %*% (y-mu)/n
    I<-matrix(0,ncol=n,nrow=n)
    for(i in 1: n)
      I[i,i]<-1
    GCV<-(n^2*MSE)/(sum(diag(I-h)))^2
    Gcv[j]<-GCV
    Mse[j]<-MSE
  }
  R<-matrix(c(k,Gcv,Mse),ncol=3)
	plot(k,Gcv,col="blue",type="l",xlab="Titik Knot",ylab="GCV")
	title(main="PLOT GCV TERHADAP TITIK KNOT")
  jml<-length(k)
  sort.R<-R[order(R[,2]),]
  S<-sort.R[1:jml,]
  cat("Untuk spline order",m1,"dengan 1 titik knot, diperoleh knot optimal=",S[1,1]," dengan GCV minimum=",S[1,2],"dan MSE =",S[1,3])
  cat("\nBerikut urutan nilai GCV terkecil, nilai MSE dan letak titik knotnya:\n")
  cat("====================================\n")
  cat("  No  Titik knot   GCV     MSE   \n")
  cat("====================================\n")
  S
}

spline<-function(Chlorine)
{
	p<-as.numeric(readline("inputkan orde = "))
	k<-as.numeric(readline("inputkan jumlah titik knot optimal= "))
	n<-nrow(data)
	prediktor<-data[,1]
	dataurut<-data[order(prediktor),1:2]
	x1<-dataurut[,1]
	y1<-dataurut[,2]
	x<-as.matrix(x1)
	y<-as.matrix(y1)
	w<-rep(0,k)
	cat("Jumlah knot = ",k,"\n")
	for(i in 1:k)
	{
		cat("titik knot = ")
            w[i]<-as.numeric(readline(" "))
		knot<-w[i]
	}
	v1<-matrix(0,n,p+1)
	for(i in 1:p)
	{

		v1[,i]<-x^(i-1)
		v1[,p+1]<-x^(p)
	}
	v2<-matrix(0,n,k)
	for(i in 1:k)
	{
		v2[,i]<-trun(x,w[i],p)
	}
	X<-cbind(v1,v2)
	betatopi<-solve(t(X)%*%X)%*%t(X)%*%y
	ytopi<-X%*%betatopi
	H<-X%*%solve(t(X)%*%X)%*%t(X)
	MSE<-(t(y-ytopi)%*%(y-ytopi))/n
	GCV<-MSE/(1-((1/n)*sum(diag(H))))^2
	JKT<-t(y-(mean(y)))%*%(y-(mean(y)))
	JKG<-t(y-ytopi)%*%(y-ytopi)
	R2<-1-(JKG/JKT)
	for(i in 1:(p+1+k))
	{
		cat("\n nilai betatopi[ ",i,"]= ",format(betatopi[i]),"\n")
	}
	win.graph()
	plot(x,y,xlim=c(min(x),max(x)),ylim=c(min(c(y,ytopi)),max(c(y,ytopi))),xlab="Weeks",ylab="Chlorine")
	par(new=T)
	plot(x,ytopi,type="l",col="red",xlim=c(min(x),max(x)),ylim=c(min(c(y,ytopi)),max(c(y,ytopi))),xlab="Weeks",ylab="Chlorine")
	title(main="Plot Weeks terhadap Chlorine")
	cat("	Nilai MSE = ",MSE,"\n")
	cat("	Nilai GCV = ",GCV,"\n")
	cat("	Nilai R2  = ",R2,"\n")
}

