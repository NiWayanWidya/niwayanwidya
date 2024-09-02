diskrit=function()
{
  a=as.numeric(readline("Input banyak p :"))
  p=rep(0,a)
  k=rep(0,a)
  
  for(i in 1:a)
  {
   p[i]=as.numeric(readline("Input p = "))
  }
  
  for(i in i:p)
  {
    if(i==1){k[i]=p[i]}
    else{k[i]=p[i]+k[i-1]}
  }
  
  n=as.numeric(readline("Input banyak U : "))
  u=runif(n)
  x=rep(0,n)
  
  for(i in i:n)
  {
    if(u[i]<k[1]) {x[i]=1}
    else if(u[i]<k[2]) {x[i]=2}
    else if(u[i]<k[3]) {x[i]=3}
    else {x[i]=4}
  }
  print(cbind(u,x))
}
diskrit()


kontinu=function()
{
  n=as.numeric(readline("Input banyak U : "))
  u=runif(n)
  x=rep(0,n)
   for(i in 1:n)
   {
     x[i]=qnorm(u[i],0,1)
   }
  print(cbind(u,x))
}
kontinu