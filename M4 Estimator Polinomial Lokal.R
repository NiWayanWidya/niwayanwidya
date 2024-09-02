library(KernSmooth)
library(locpol)

data("geyser",package="MASS")
geyser
x<-geyser$duration
y<-geyser$waiting
kernel=gaussK
h=regCVBwSelC(x,y,2,kernel)
h
fit=locPolSmootherC(x,y,sort(x),h,2,kernel)
fit
plot(x,y)
lines(fit)

#LATIHAN

#1
Rtcovid<- read.delim("~/KULIAH/SMT 5/ANALISIS REGRESI NONPARAMETRIK/PositifRateCovid.txt")
View(Rtcovid)
t=Rtcovid$t
Rt=Rtcovid$y
plot(t,Rt)
kernel1=CosK
kernel2=TriweigK
kernel3=TrianK
kernel4=gaussK
#a
h01=regCVBwSelC(t,Rt,0,kernel1)
h01
h02=regCVBwSelC(t,Rt,0,kernel2)
h02
h03=regCVBwSelC(t,Rt,0,kernel3)
h03
h04=regCVBwSelC(t,Rt,0,kernel4)
h04
#b
h11=regCVBwSelC(t,Rt,1,kernel1)
h11
h12=regCVBwSelC(t,Rt,1,kernel2)
h12
h13=regCVBwSelC(t,Rt,1,kernel3)
h13
h14=regCVBwSelC(t,Rt,1,kernel4)
h14
#c
h21=regCVBwSelC(t,Rt,2,kernel1)
h21
h22=regCVBwSelC(t,Rt,2,kernel2)
h22
h23=regCVBwSelC(t,Rt,2,kernel3)
h23
h24=regCVBwSelC(t,Rt,2,kernel4)
h24

#2
#kernel CosK
#jika derajat polinomial 0
fit01=locPolSmootherC(t,Rt,sort(t),h01,0,kernel1)
fit01
plot(t,Rt)
lines(fit01)
#jika derajat polinomial 1
fit11=locPolSmootherC(t,Rt,sort(t),h11,1,kernel1)
fit11
plot(t,Rt)
lines(fit11)
#jika derajat polinomial 2
fit21=locPolSmootherC(t,Rt,sort(t),h21,2,kernel1)
fit21
plot(t,Rt)
lines(fit21)

#kernel TriweigK
#jika derajat polinomial 0
fit02=locPolSmootherC(t,Rt,sort(t),h02,0,kernel2)
fit02
plot(t,Rt)
lines(fit02)
#jika derajat polinomial 1
fit12=locPolSmootherC(t,Rt,sort(t),h12,1,kernel2)
fit12
plot(t,Rt)
lines(fit12)
#jika derajat polinomial 2
fit22=locPolSmootherC(t,Rt,sort(t),h22,2,kernel2)
fit22
plot(t,Rt)
lines(fit22)

#kernel TrianK
#jika derajat polinomial 0
fit03=locPolSmootherC(t,Rt,sort(t),h03,0,kernel3)
fit03
plot(t,Rt)
lines(fit03)
#jika derajat polinomial 1
fit13=locPolSmootherC(t,Rt,sort(t),h13,1,kernel3)
fit13
plot(t,Rt)
lines(fit13)
#jika derajat polinomial 2
fit23=locPolSmootherC(t,Rt,sort(t),h23,2,kernel3)
fit23
plot(t,Rt)
lines(fit23)

#kernel GaussK
#jika derajat polinomial 0
fit04=locPolSmootherC(t,Rt,sort(t),h04,0,kernel4)
fit04
plot(t,Rt)
lines(fit04)
#jika derajat polinomial 1
fit14=locPolSmootherC(t,Rt,sort(t),h14,1,kernel4)
fit14
plot(t,Rt)
lines(fit14)
#jika derajat polinomial 2
fit24=locPolSmootherC(t,Rt,sort(t),h24,2,kernel4)
fit24
plot(t,Rt)
lines(fit24)