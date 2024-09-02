{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
GWR<-read.csv("C:/Users/widyaseptia/Documents/KULIAH/SMT 6/Analisis Data Spasial/data ipm .csv")
GWR

#Uji Spatial Dependency
library(ape)
matriks.jarak<-as.matrix(dist(cbind(data.ipm$u, data.ipm$v)))
matriks.bobot<-1/matriks.jarak
diag(matriks.bobot)<-0
Moran.I(data.ipm$Y, matriks.bobot)
```


```{r}
#Uji Spatial Heterogenity
library(lmtest)
regresi<-lm(Y~X1+X2+X3+X4, data=data.ipm)
summary(regresi)
bptest(regresi)
```