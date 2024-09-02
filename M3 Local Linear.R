library(KernSmooth)
library(MASS)
library(locpol)

data("geyser",package="MASS")
geyser
x<-geyser$duration
y<-geyser$waiting
h1=regCVBwSelC(x,y,1,kernel=gaussK)
h1
h2=pluginBw(x,y,1,kernel=gaussK)
h2
h3=thumbBw(x,y,1,kernel=gaussK)
h3


data("geyser",package="MASS")
geyser
x<-geyser$duration
y<-geyser$waiting
h1=regCVBwSelC(x,y,1,kernel=gaussK)
h1
h2=pluginBw(x,y,1,kernel=gaussK)
h2
h3=thumbBw(x,y,1,kernel=gaussK)
h3
xurut<-sort(x)
h=pluginBw(x,y,1,kernel=gaussK)
h
estimasiLoclin=locLinSmootherC(x,y,xurut,h,kernel=gaussK)
estimasiLoclin
yduga=estimasiLoclin[,2]
yduga
plot(x,y)
lines(xurut,yduga)

estimasiLoclin=locLinSmootherC(x,y,0.95,h,kernel=gaussK)
estimasiLoclin


library(readxl)
data <- read_excel("C:/Users/acer/OneDrive/Desktop/Abalone.xls")
data
abalone <- data[c(1:100),]
abalone
x<-abalone$diameter
y<-abalone$Rings
xurut<-sort(x)
h=regCVBwSelC(x,y,1,kernel=CosK)
h
estimasiLoclin=locLinSmootherC(x,y,xurut,h,kernel=CosK)
estimasiLoclin
yduga=estimasiLoclin[,2]
yduga
plot(x,y)
lines(xurut,yduga)

data1<-data(Abalone)
Abalone<-data1[c(1:100),]
x<-Abalone$diameter
Y<-Abalone$Rings
h1=regCVBwSelC(x,y,1,kernel=CosK)
h1

#latihan

#1
library(readxl)
data <- read_excel("C:/Users/acer/OneDrive/Desktop/Abalone.xls")
data
abalone <- data[c(1:100),]
abalone
x<-abalone$diameter
y<-abalone$Rings
xurut<-sort(x)


#a CV,CosK
h=regCVBwSelC(x,y,1,kernel=CosK)
h
estimasiLoclin=locLinSmootherC(x,y,xurut,h,kernel=CosK)
estimasiLoclin
yduga=estimasiLoclin[,2]
yduga
plot(x,y)
lines(xurut,yduga)

MSE<-(sum((y-yduga)^2))/100
MSE

R2 <- sum((yduga-mean(y))^2)/sum((y-mean(y))^2)
R2
