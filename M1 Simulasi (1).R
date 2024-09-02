

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


