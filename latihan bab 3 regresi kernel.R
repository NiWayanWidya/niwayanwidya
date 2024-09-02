library(KernSmooth)
library(locpol)
library(MASS)
library(PLRModels)
#Program untuk Regresi Kernel

#a) Program Awal
data(geyser, package="MASS") #Pemanggilan data
geyser_data = geyser[order(geyser$duration),] #Pengurutan Data berdasarkan X
x = geyser_data$duration
y = geyser_data$waiting

#b) Penentuan Bandwith dengan CV
h = regCVBwSelC(x,y,0, kernel=gaussK)
print(h)

#c) Penentuan Bandwith dengan GCV
matrix_1 = cbind(y,x)
GCV = np.gcv(matrix_1,estimator = "NW",kernel="gaussian")
h = GCV$h.opt
print(h)
plot(GCV$h.seq, GCV$GCV, type="l") #Hasil Plotting

#d) Penentuan Bandwith dengan metode Rule of Thumb
h = thumbBw(x,y,0, kernel=gaussK)
print(h)
##KETERANGAN : PILIH SALAH SATU METODE DALAM PENENTUAN BANDWITH!

#e) Estimasi Regresi Kernel
Estimated_kernel = locCteSmootherC(x,y,x,h,kernel=gaussK)
y_est = Estimated_kernel$beta0
matrix_est = cbind(x,y,y_est)
plot(geyser$duration,geyser$waiting)
lines(x,y_est)

#f) Perhitungan MSE dan R^2
MSE = sum((y-y_est)^2)/length(y)
R_sq = 1 - sum((y_est-y)^2)/sum((y-mean(y))^2)
Goodness = c(MSE,R_sq)
print(Goodness)
