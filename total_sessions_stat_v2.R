setwd("C:/Users/Lily/Documents/GA/R/report/2017/Monthly/Q1/total_dat/")
cname <- "Australia"
clist <- list("Australia","Austria","Belgium","Canada","Czechia","Denmark","France","Germany","Greece","Hong Kong","Hungary","India","Iran","Israel","Italy","Japan","Mexico","Netherlands","Norway","Poland","Portugal","Romania","South Africa","South Korea","Spain","Sweden","Switzerland","Taiwan","Thailand","Turkey","United Kingdom","United States")
stat <- function(cname){
  Q1_data <- read.csv(paste(cname, '.csv', sep=""), header=T)
  Q1_data$day <- weekdays.Date(as.Date(as.character(Q1_data$date), format="%Y%m%d"))
  day_trans <- matrix(0,7,2)
  day_trans[,1] <- c(1:7)
  day_trans[,2] <- c("星期一", "星期二", "星期三", "星期四", "星期五", "星期六", "星期日")
  Q1_data$day <- match(Q1_data$day[], day_trans[,2])
  len <- length(Q1_data$sessions)-6
  CMA=numeric(len)  
  for(i in 1:len)
  { 
    CMA[i]=(Q1_data[i,3]+2*Q1_data[i+1,3]+Q1_data[i+2,3]+Q1_data[i+3,3]+Q1_data[i+4,3]+Q1_data[i+5,3]+Q1_data[i+6,3])/7
  }
  Q1_data$CMA <- c(rep(0,3), CMA, rep(0,3))
  # Q1_data$d=Q1_data$sessions-Q1_data$CMA
  # # seasonal index
  # dm=tapply(Q1_data$d, Q1_data$day, mean, na.rm =T) 
  # sdm=sum(dm) 
  # index=dm-sdm/7 ; index
  # # seasonal index
  # Q1_data$S=c(index[7], rep(index, 12), index[1:5])
  # # plot(Q1_data$S, type= "b")
  # # y (去除季節指數的數列)
  # Q1_data$Y=Q1_data$sessions-Q1_data$S
  # # plot(Q1_data$Y, type= "b")
  # lines(Q1_data$Y, type="b", col="blue")
  std <- round(sd(CMA),4)
  mean <- round(mean(CMA),2)
  upper <- round((mean + 1.5*std),2)
  lower <- round((mean - 1.5*std),2)
  png("../res.png", width = 800, height = 450, units = "px")
  plot(Q1_data$sessions, type= "l")
  lines(Q1_data$CMA, type="l", col="red")
  abline(a=mean, b=0, col=4)
  abline(a=upper, b=0, col=5)  
  abline(a=lower, b=0, col=5)
  dev.off()
  
  bi_data <- read.csv(paste("C:/Users/Lily/Documents/GA/R/report/2017/bi-weekly/0529-0609/total_dat/", cname, ".csv", sep=""), header=T)
  bi_data$day <- weekdays.Date(as.Date(as.character(bi_data$date), format="%Y%m%d"))
  bi_data$day <- match(bi_data$day[], day_trans[,2])
  bi_len <- length(bi_data$sessions)-6
  bi_CMA=numeric(bi_len)  
  for(i in 1:bi_len)
  { 
    bi_CMA[i]=(bi_data[i,3]+2*bi_data[i+1,3]+bi_data[i+2,3]+bi_data[i+3,3]+bi_data[i+4,3]+bi_data[i+5,3]+bi_data[i+6,3])/7
  }
  bi_data$CMA <- c(rep(0,3), bi_CMA, rep(0,3))
  plot(bi_data$sessions, type= "b")
  lines(bi_data$CMA, type="b", col="red")
  bi_std <- round(sd(bi_CMA),4)
  bi_mean <- round(mean(bi_CMA),2)
  bi_U <- round((bi_mean + 1*bi_std),2)
  bi_L <- round((bi_mean - 1*bi_std),2)
  
  if(bi_U > upper){
    U_alert <- "+"
    L_alert <- ""
    if(bi_L < lower){
      L_alert <- "-"
    }
  }else if(bi_L < lower){
    L_alert <- "-"
    U_alert <- ""
    if(bi_U > upper){
      U_alert <- "+"
    }
  }else{
    U_alert <- ""
    L_alert <- ""
  }
  U_dif <- abs(bi_U-upper)
  L_dif <- abs(bi_L-lower)
  # maxi_bi <- max(bi_data$sessions)
  # mini_bi <- min(bi_data$sessions)
  # if(maxi_bi>upper){
  #   U_alert <- "+"
  #   L_alert <- ""
  #   if(mini_bi < lower){
  #     L_alert <- "-"
  #   }
  # }else if(mini_bi < lower){
  #   L_alert <- "-"
  #   U_alert <- ""
  #   if(maxi_bi > upper){
  #     U_alert <- "+"
  #   }
  # }else{
  #   U_alert <- ""
  #   L_alert <- ""
  # }
  # U_dif <- abs(maxi_bi-upper)
  # L_dif <- abs(mini_bi-lower)
  return(c(mean, std, upper,lower, bi_mean, bi_std, bi_U, bi_L, U_dif, L_dif, U_alert, L_alert))
}
stat(cname)
A <- as.data.frame(lapply(clist, stat))
colnames(A) <- clist
B <- t(A)
colnames(B) <- c("mean", "std", "upper", "lower", "bi_mean", "bi_std", "bi_U", "bi_L", "U_dif", "L_dif", "U_alert", "L_alert")
C <- as.data.frame(B)
lan_list<- c("en-au", "", "", "", "cs-cz", "", "fr-fr", "de-de", "", "zh-hk", "", "en-in", "", "", "it-it", "ja-jp", "es-mx", "nl-nl", "", "pl-pl", "pt-pt", "", "", "ko-kr", "es-es", "sv-se", "", "zh-tw", "th-th", "", "en-uk", "en-us")
C$lan <- lan_list
as.data.frame(C)
write.csv(C, 'C:/Users/Lily/Documents/GA/R/report/2017/bi-weekly/0529-0609/stat_lan_v2.csv')
