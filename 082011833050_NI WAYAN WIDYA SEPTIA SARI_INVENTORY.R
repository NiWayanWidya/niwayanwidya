TugasInventori <- function(rp,oq,dmdm,dmdsd,pc,sc1,hc1,oc1,shc,bi,n)
{
  cat("=====================================TUGAS SIMULASI S3==================================\n")
  cat("NAMA : NI WAYAN WIDYA SEPTIA SARI\n")
  cat("NIM  : 082011833050\n")
  cat("========================================================================================\n")
  recv <- rep(0,n)
  stock <- rep(0,n)
  stock[1] <- bi
  demand <- round(rnorm(n,dmdm,dmdsd))
  sold <- rep(0,n)
  sisa <- rep(0,n)
  order <- rep(0,n)
  oc <- rep(0,n)
  hc <- rep(0,n)
  sc <- rep(0,n)
  purch <- rep(0,n)
  tcost <- rep(0,n)
  rev <- rep(0,n)
  profit <- rep(0,n)
  
  for (i in 1:n) {
    #recv
    if (i != 1){
      if (order[i-1]==1){recv[i]=oq}
      else {recv[i]==0}
    }
    #stock
    if (i != 1){stock[i] = recv[i]+sisa[i-1]}
    #sold
    sold[i]=min(demand[i],stock[i])
    #sisa
    sisa[i]=stock[i]-sold[i]
    #order
    if (sisa[i]<rp){order[i]=1}
    #OC
    if (order[i]==1){oc[i]=oc1}
    #HC
    hc[i]=sisa[i]*hc1
    #SC
    if (stock[i] < demand[i]){sc[i]=(demand[i]-stock[i])*sc1}
    #purch
    if (recv[i] != 0) {purch[i]=pc*recv[i]}
    #tcost
    tcost[i]=oc[i]+hc[i]+sc[i]+purch[i]
    #rev
    rev[i] = sold[i] * sc1
    #profit
    profit[i]=rev[i] - tcost[i]
  }
  print(data.frame(recv,stock,demand,sold,sisa,order,oc,hc,sc,purch,tcost,rev,profit))
  cat("rata-rata profit = ",mean(profit))
}
TugasInventori(700,1200,800,60,15,30,1,100,1.5,750,8)