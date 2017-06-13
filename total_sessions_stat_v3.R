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
    CMA[i]=(Q1_data[i,3]+Q1_data[i+1,3]+Q1_data[i+2,3]+Q1_data[i+3,3]+Q1_data[i+4,3]+Q1_data[i+5,3]+Q1_data[i+6,3])/7
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
  png(paste("../", cname, "res.png"), width = 800, height = 450, units = "px")
  plot(Q1_data$sessions, type= "l")
  lines(Q1_data$CMA, type="l", col="red")
  abline(a=mean, b=0, col=4)
  abline(a=upper, b=0, col=3)  
  abline(a=lower, b=0, col=3)
  dev.off()
  
  M4_data <- read.csv(paste("C:/Users/Lily/Documents/GA/R/report/2017/Monthly/April/total_dat/", cname, '.csv', sep=""), header=T)
  M4_data$day <- weekdays.Date(as.Date(as.character(M4_data$date), format="%Y%m%d"))
  M4_data$day <- match(M4_data$day[], day_trans[,2])
  M4_len <- length(M4_data$sessions)-6
  M4_CMA=numeric(len)  
  for(i in 1:M4_len)
  { 
    M4_CMA[i]=(M4_data[i,3]+M4_data[i+1,3]+M4_data[i+2,3]+M4_data[i+3,3]+M4_data[i+4,3]+M4_data[i+5,3]+M4_data[i+6,3])/7
  }
  M4_data$CMA <- c(rep(0,3), M4_CMA, rep(0,2))
  png(paste("../", cname, "_M4.png"), width = 800, height = 450, units = "px")
  plot(M4_data$sessions, type= "l")
  lines(M4_data$CMA, type="l", col="red")
  abline(a=mean, b=0, col=4)
  abline(a=upper, b=0, col=3)  
  abline(a=lower, b=0, col=3)
  dev.off()
  
  M5_data <- read.csv(paste("C:/Users/Lily/Documents/GA/R/report/2017/Monthly/May/total_dat/", cname, '.csv', sep=""), header=T)
  M5_data$day <- weekdays.Date(as.Date(as.character(M5_data$date), format="%Y%m%d"))
  M5_data$day <- match(M5_data$day[], day_trans[,2])
  M5_len <- length(M5_data$sessions)-6
  M5_CMA=numeric(len)  
  for(i in 1:M5_len)
  { 
    M5_CMA[i]=(M5_data[i,3]+M5_data[i+1,3]+M5_data[i+2,3]+M5_data[i+3,3]+M5_data[i+4,3]+M5_data[i+5,3]+M5_data[i+6,3])/7
  }
  M5_data$CMA <- c(rep(0,3), M5_CMA, rep(0,3))
  png(paste("../", cname, "_M5.png"), width = 800, height = 450, units = "px")
  plot(M5_data$sessions, type= "l")
  lines(M5_data$CMA, type="l", col="red")
  abline(a=mean, b=0, col=4)
  abline(a=upper, b=0, col=3)  
  abline(a=lower, b=0, col=3)
  dev.off()
  # return(c(mean, std, upper,lower, bi_mean, bi_std, bi_U, bi_L, U_dif, L_dif, U_alert, L_alert))
  return()
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
