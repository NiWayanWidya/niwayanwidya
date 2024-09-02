#Program Gnerate Acak Distribusi Diskrit 
p= c(0.2, 0.15, 0.25, 0.4)
random_uniform = runif(5)

random_x=as.vector(rep(0,5))
for(i in 1:5)
{
  if(random_unifrom[i]<p[1])
  {
    random_x=1
  }
  else if(random_uniform[i]<p[1]+p[2])
  {
    random_x[i]=2
  }
  else if(random_uniform[i]<p[1]+p[2]+p[3])
  {
    random_x[i]=3
  }
  else
  {
    random_x[1]=4
  }
}
for(i in 1:5)
{
  cat(random_uniform[i],"\t\t", random_x[i],"\n")
}



#Program Gnerate Acak Distribusi Kontinu

u2<- 10

data_unif2<-runif(u2,0,1)
data_unif2

hasilinv= rep(0,u2)
for(i in 1:u2){
  hasilinv[i] = - log (1- data_unif2[i])
}
  
for (i : in 1:u2){
  cat(data_unif2[i], "\t\t" , hasilinv[i], "\n")
}
  