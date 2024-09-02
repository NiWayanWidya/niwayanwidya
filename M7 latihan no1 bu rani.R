n<-1000; a<-3;b<-2;
u<-rgamma(n,shape=a,rate=1);
u
v<-rgamma(n,shape=b, rate=1);
v
x<- u/(u+ v)

randgamma<-function(n,a,b)
{
  u<-rgamma(n,shape=a,rate=1);
  y<-rgamma(n,shape=b, rate=1);
  x<- u/( u + v)
  print(x)
}
randgamma(1000,3,2)