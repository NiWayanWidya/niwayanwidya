#contoh 
{r}
library(KernSmooth)
library(locpol)

{r}
data(geyser,package = "MASS")
x<-geyser$duration
y<-geyser$waiting
h=regCVBwSelC(x,y,2,kernel=gaussK)
h

{r}
fit=locPolSmootherC(x,y,sort(x),h,2,kernel=gaussK)
plot(x,y)
lines(fit)

#latihan 
#a
library(KernSmooth)
library(locpol)
x<-PositifRateCovid$t
y<-PositifRateCovid$y
kernel= CosK
h=regCVBwSelC(x,y,0,kernel)
h

#b
library(KernSmooth)
library(locpol)
x<-PositifRateCovid$t
y<-PositifRateCovid$y
kernel= TriweigK
h=regCVBwSelC(x,y,0,kernel)
h

#C
library(KernSmooth)
library(locpol)
x<-PositifRateCovid$t
y<-PositifRateCovid$y
kernel= TrianK
h=regCVBwSelC(x,y,0,kernel)
h

#D
library(KernSmooth)
library(locpol)
x<-PositifRateCovid$t
y<-PositifRateCovid$y
kernel= gaussK
h=regCVBwSelC(x,y,0,kernel)
h

#2
library(KernSmooth)
library(locpol)
x <- PositifRateCovid$t
y <- PositifRateCovid$y
kernel = gaussK
h0 = regCVBwSelC(x,y,0,kernel)
h1 = regCVBwSelC(x,y,1,kernel)
h2 = regCVBwSelC(x,y,2,kernel)
h0 
h1 
h2
fit0 = locPolSmootherC(x,y,sort(x),h0,0,kernel)
fit1 = locPolSmootherC(x,y,sort(x),h1,1,kernel)
fit2 = locPolSmootherC(x,y,sort(x),h2,2,kernel)
fit0
fit1
fit2
plot(x,y)
lines(fit)
par(mfrow=c(3,1))
plotho = plot(x,y)+lines(fit0)
ploth1 = plot(x,y)+lines(fit1)
ploth2 = plot(x,y)+lines(fit2)
MSEho=(sum((fit0[,2]-y)^2)/length(x))
MSEho
MSEh1=(sum((fit1[,2]-y)^2)/length(x))
MSEh1
MSEh2=(sum((fit2[,2]-y)^2)/length(x))
MSEh2

