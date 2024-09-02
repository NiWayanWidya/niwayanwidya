#Data
library(readxl)
data <- read_excel("C:/Users/widyaseptia/Documents/KULIAH/SMT 6/Analisis Data Spasial/data diabetes 21.xlsx")
View(data)

#Breusch-Pagan Test
library(lmtest)
modelpoisson <- lm(y~x1+x2+x3, data = data) 
bptest(modelpoisson)

#Uji Spasial Dependency
library(ape)
matriks=as.matrix(dist(cbind(data$v, data$u)))
matriks.inv = 1/matriks
diag(matriks.inv)=0
Moran.I(data$y, matriks.inv)