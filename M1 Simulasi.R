

Departure_Time <- function()
{
  Arrival_Times <- matrix(c(12,31,63,95,99,154,198,221,304,346,411,455,537))
  Service_Times <- matrix(c(40,32,55,48,18,50,47,18,28,54,40,72,12))
  n <- length(Arrival_Times)
  Departure_Time1 <- 0
  Departure_Time2 <- 0 
  for(i in 1:n){
    if(Departure_Time1 < Arrival_Times[i]){
      Departure_Time1 = Arrival_Times [i] + Service_Times [i]
      print(paste("Pelanggan ke-",i,"berada pada server 1 dan meninggalkan layanan pada menit ke-",Departure_Time1))
    }else if(Arrival_Times[i] < Departure_Time2){
      Departure_Time1 = Departure_Time1 + Service_Times [i]
      print(paste("Pelanggan ke-",i,"berada pada server 1 dan meninggalkan layanan pada menit ke-",Departure_Time1))
    }else if(Arrival_Times[i] < Departure_Time1){
      Departure_Time2 = Arrival_Times [i] + Service_Times [i]
      print(paste("Pelanggan ke-",i,"berada pada server 2 dan meninggalkan layanan pada menit ke-",Departure_Time2))
    }
  }
}

Departure_Time()

arr=c(12,31,63,95,99,154,198,221,304,346,411,455,537)
serv=c(40,32,55,48,18,50,47,18,28,54,40,72,12)

#a
exercise_a=function(arrivaltime, servicetime)
{
  if (length(arrivaltime) != length(servicetime))
  {print("banyak data tidak sama")}
  n=length(arrivaltime)
  serveremptytime=0
  departuretime=rep(1,n)
  for (i in 1:n)
  {
    if (serveremptytime < arrivaltime[i])
      departuretime[i]=arrivaltime[i]+servicetime[i]
    else
      departuretime[i]=serveremptytime+servicetime[i]
    serveremptytime=departuretime[i]
  }
  print(departuretime)
}

#b
exercise_b=function(arrivaltime, servicetime)
{
  if (length(arrivaltime) != length(servicetime))
  {print("banyak data tidak sama")}
  n=length(arrivaltime)
  s=2
  serveremptytime=rep(1,s) #waktu ketika server ke-i kosong
  departuretime=rep(1,n)
  for (i in 1:n)
    #servertime adalah waktu paling awal ketika server kosong
    #server_no adalah nomor server (1 atau 2)  
  {
    servertime=min(serveremptytime)
    server_no=which.min(serveremptytime)
    if (servertime < arrivaltime[i])
      departuretime[i]=arrivaltime[i]+servicetime[i]
    else
      departuretime[i]=servertime+servicetime[i]
    serveremptytime[server_no]=departuretime[i]
  }
  print(departuretime)
}

#c
exercise_C=function(arrivaltime, servicetime)
{
  if (length(arrivaltime) != length(servicetime))
  {print("banyak data tidak sama")}
  n=length(arrivaltime)
  serveremptytime=0
  departuretime=rep(1,n)
  waitingtime=rep(1,n)
  for (i in 1:n)
  {
    if (serveremptytime < arrivaltime[i])
    {departuretime[i]=arrivaltime[i]+servicetime[i]
    waitingtime[i]=0}
    else
    {departuretime[i]=serveremptytime+servicetime[i]
    waitingtime[i]=serveremptytime-arrivaltime[i]}
    serveremptytime=departuretime[i]
  }
  cat("  ", "departuretime \n")
  cat("  ", departuretime,"\n")
  cat("  ", "waitingtime \n")
  cat("  ", waitingtime, "\n")
}
