#NAMA : Ni Wayan Widya Septia Sari
#NIM  : 082011833050

#UTS STAT KOM PRAK S2 
#NO 2
no2<- function()
{
  a = -3
  b = 3
  n <- as.numeric(readline("Input banyaknya nilai x : "))
  cat ("Input nilai x \n")
  x <- scan(n=n)
  
  if(x<0)
  {
    fx <- x^2+2*x+3
  }
  else if(x>=0 && x<2)
  {
    fx <- x+3
  }else if(x>=2)
  {
    fx <- x^2+4*x-7
  }
  cat("Nilai fx :",fx,"\n")
  plot(x,fx,type="l",xlab="x",ylab="fx")
}
no2()