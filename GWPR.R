#Data
library(readxl)
tbc <- read_excel("C:/Users/widyaseptia/Downloads/coba data kusta (1).xlsx")
View(tbc)

#Breusch-Pagan Test
library(lmtest)
modelpoisson=lm(y~x1+x2+x3,data=tbc) 
bptest(modelpoisson)

#Uji Spasial Dependency
library(ape)
matriks=as.matrix(dist(cbind(tbc$v, tbc$u)))
matriks.inv = 1/matriks
diag(matriks.inv)=0
Moran.I(tbc$y, matriks.inv)