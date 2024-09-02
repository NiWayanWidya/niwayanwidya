#input#
f = c(5,10,15,30,25,15)
xx = c(4,5,6,7,8,9)
montecarlo = function(x,frekk)
{
  
  dist.prob = frekk/sum(frekk)
  dist.prob
  dist.kum = cumsum(dist.prob)
  dist.kum
  set.seed(10)
  u = runif(10)
  output = array(NA,dim = c (10,2))
  for(i in 1 : 10)
  {
    output[i,1]=i
    if(u[i]<=dist.kum[1])
    {
      output[i,2]= 5
    }
    else if (u[i]<=dist.kum[2])
    {
      output[i,2]= 10
    } 
    else if (u[i]<=dist.kum[3])
    {
      output[i,2]= 15
    } 
    else if(u[i]<=dist.kum[4])
    {
      output[i,2]= 30
    } 
    else if (u[i]<=dist.kum[5])
    {
      output[i,2]= 25
    } 
    else 
    {
      output[i,2]= 15  } 
  }
  output
  
  # membuat data frame 
  hari = output[,1]
  hari
  prediksi_permintaan = output[,2]
  prediksi_permintaan
  table_prediksi_permintaan = data.frame(hari,prediksi_permintaan)
  table_prediksi_permintaan
}

montecarlo(xx,f)


