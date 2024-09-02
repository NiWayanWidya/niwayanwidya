install.packages("KernSmooth")
install.packages("locpol")
library(KernSmooth)
library(locpol)
library(MASS)

data("geyser", package = "MASS")
x<-geyser$duration
y<-geyser$waiting
h=regCVBwSelC(x, y, 0, kernel=gaussK)
h
xurut<-sort(x)
estimasiKernel=locCteSmootherC(x, y, xurut, h, kernel = gaussK)
yduga=estimasiKernel[,2]
plot(x,y)
lines(xurut, yduga)

#LATIHAN DATA ABALONE
library(readxl)
abalone <- read_excel("C:/Users/acer/OneDrive/Desktop/Data Abalone.xls",sheet = 1)
abalone


#Soal Latihan No.4
#Untuk data preparation
abalone <- read.table(file = "data.csv", sep = ",", header = T)
x <- head(abalone$Whole,100)
y <- head(abalone$Rings,100)
matrixnya <- cbind(x,y)

#library
library(PLRModels)

#step-by-step
a <-np.gcv(matrixnya)
a$GCV.opt

GCV <- a$GCV
h <- a$h.seq
plot(h, GCV, type="l")