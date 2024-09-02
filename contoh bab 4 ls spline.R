library(KernSmooth)
library(MASS)
library(locpol)
data("geyser",package="MASS")
x<-geyser$duration
y<-geyser$waiting
h1=regCVBwSelC(x,y,1,kernel=gaussK)
h1
h2=pluginBw(x,y,1,kernel=gaussK)
h2
h3=thumbBw(x,y,1,kernel=gaussK)
h3

library(KernSmooth)
library(MASS)
library(locpol)
data("geyser",package="MASS")
x<-geyser$duration
y<-geyser$waiting
xurut<-sort(x)
h=pluginBw(x,y,1,kernel=gaussK)
estimasiLoclin=locLinSmootherC(x,y,xurut,h,kernel=gaussK)
yduga=estimasiLoclin[,2]
yduga
plot(x,y)
lines(xurut,yduga)

estimasiLoclin2=locLinSmootherC(x,y,0,95,h,kernel=gaussK)
estimasiLoclin2


