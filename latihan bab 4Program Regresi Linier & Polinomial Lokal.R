library(KernSmooth)
library(locpol)
library(MASS)
library(PLRModels)
library(fANCOVA)
#Program untuk Regresi Polinomial Lokal (Linier Lokal Juga xixixi)

#a) Program Awal
data(geyser, package="MASS") #Pemanggilan data
geyser_data = geyser[order(geyser$duration),] #Pengurutan Data berdasarkan X
x = geyser_data$duration
y = geyser_data$waiting

#b) Penentuan Bandwith dengan CV
h = regCVBwSelC(x,y,2, kernel=gaussK) #degreenya bisa 1,2,..dst mengikuti derajat regresi
print(h)

#c) Penentuan Bandwith dengan metode Rule of Thumb
h = thumbBw(x,y,2, kernel=gaussK)
print(h)

#d) Penentuan Bandwith dengan metode Plug-In
h = pluginBw(x,y,2, kernel=gaussK) #Untuk Derajat Ganjil
print(h)
##KETERANGAN : PILIH SALAH SATU METODE DALAM PENENTUAN BANDWITH!

#e) Estimasi Regresi Polinomial Lokal
Estimated_pol = locPolSmootherC(x,y,x,h,2,kernel=gaussK) #degreenya bisa 1,2,..dst mengikuti derajat regresi
y_est = Estimated_pol$beta0
matrix_est = cbind(x,y,y_est)
plot(geyser$duration,geyser$waiting)
lines(x,y_est)

#f) Estimasi Regresi Polinomial Lokal dengan GCV
Estimated_pol = loess.as(x,y,degree=2,criterion=c("gcv")
                         ,plot=TRUE)
y_est = Estimated_pol$fitted
matrix_est = cbind(x,y,y_est)

#f) Perhitungan MSE dan R^2
MSE = sum((y-y_est)^2)/length(y)
R_sq = 1 - sum((y_est-y)^2)/sum((y-mean(y))^2)
Goodness = c(MSE,R_sq)
print(Goodness)

