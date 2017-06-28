library(ggplot2)
library(scales)
# make directory
# mdir <- "mkdir C:\\Users\\Lily\\Documents\\GA\\R\\report\\2017\\Monthly\\trend\\"
# fnames <- "May"
# f_list <- unlist(lapply(mdir, paste, fnames, sep=""))
# lapply(f_list, shell)

# set working directory
setwd("C:/Users/Lily/Documents/GA/R/report/2017/Monthly/trend/")

# cname <- "Australia"
# mon <- "Q1"
clist <- list("Australia","Austria","Belgium","Canada","Czechia","Denmark","France","Germany","Greece","Hong Kong","Hungary","India","Iran","Israel","Italy","Japan","Mexico","Netherlands","Norway","Poland","Portugal","Romania","South Africa","South Korea","Spain","Sweden","Switzerland","Taiwan","Thailand","Turkey","United Kingdom","United States")

#每月圖表 function
trend_mon <- function(cname, mon){
  Q1_data <- read.csv(paste("C:/Users/Lily/Documents/GA/R/report/2017/Monthly/Q1/total_dat/", cname, '.csv', sep=""), header=T)
  # Q1_data$day <- weekdays.Date(as.Date(as.character(Q1_data$date), format="%Y%m%d"))
  # day_trans <- matrix(0,7,2)
  # day_trans[,1] <- c(1:7)
  # day_trans[,2] <- c("星期一", "星期二", "星期三", "星期四", "星期五", "星期六", "星期日")
  Q1_data$day <- format(as.Date(as.character(Q1_data$date), format="%Y%m%d"), "%w")
  len <- length(Q1_data$sessions)-6
  CMA=numeric(len)  
  for(i in 1:len)
  { 
    CMA[i]=(Q1_data[i,3]+Q1_data[i+1,3]+Q1_data[i+2,3]+Q1_data[i+3,3]+Q1_data[i+4,3]+Q1_data[i+5,3]+Q1_data[i+6,3])/7
  }
  Q1_data$CMA <- c(rep(NA,3), CMA, rep(NA,3))
  std <- round(sd(CMA),4)
  mean <- round(mean(CMA),2)
  upper <- round((mean + 2*std),2)
  lower <- round((mean - 2*std),2)
  
  M_data <- read.csv(paste("C:/Users/Lily/Documents/GA/R/report/2017/Monthly/", mon, "/total_dat/", cname, '.csv', sep=""), header=T)
  M_data$day <- format(as.Date(as.character(M_data$date), format="%Y%m%d"), "%w")
  # M_data$day <- match(M_data$day[], day_trans[,2])
  M_len <- length(M_data$sessions)-6
  M_CMA=numeric(M_len)  
  for(i in 1:M_len)
  { 
    M_CMA[i]=(M_data[i,3]+M_data[i+1,3]+M_data[i+2,3]+M_data[i+3,3]+M_data[i+4,3]+M_data[i+5,3]+M_data[i+6,3])/7
  }
  M_data$CMA <- c(rep(NA,3), M_CMA, rep(NA,3))
  # png(paste("../", mon, "/", cname, ".png", sep=""), width = 800, height = 450, units = "px")
  # plot(M_data$sessions, type= "l")
  # lines(M_data$CMA, type="l", col="red")
  # abline(a=mean, b=0, col=4)
  # abline(a=upper, b=0, col=3)  
  # abline(a=lower, b=0, col=3)
  # dev.off()
  ####date=as.character(thisdata$date)
  M_data$date <- as.Date(as.character(M_data$date), format="%Y%m%d")
  p <- ggplot(M_data, aes(x1=sessions, x2=CMA))+#(M_data, aes(x=M_data$date, y=M_data$CMA))+#, linetype=country)) + 
    geom_line(aes(x=M_data$date, y=M_data$sessions, colour="sessions"), M_data)+
    geom_line(aes(x=M_data$date, y=M_data$CMA, colour="CMA"), M_data)+
    geom_hline(yintercept=mean, color="#0099FF")+
    geom_hline(yintercept=upper, color="#3399FF", linetype="dashed")+ #linetype="dashed"
    geom_hline(yintercept=lower, color="#3399FF", linetype="dashed")+
    scale_x_date(labels = date_format("%m%d"))+
    labs(title = paste(cname)
         , x = "date"
         , y = "sessions")+
    scale_colour_manual("legend", values = c("#CC66FF", "#333333")) 
  ggsave(paste(mon, "/", cname, ".png", sep=""),p)
    
  ###
  lastMonNumber <- format(M_data$date, "%m")[length(M_data$date)]
  last_mon <- M_data[format(M_data$date, "%m")==lastMonNumber,5]
  # print(last_mon)
  last_mon <- last_mon[!is.na(last_mon)]
  # print(last_mon)
  U_count <- sum(last_mon > upper)
  L_count <- sum(last_mon < lower)
  if(U_count>1 & L_count>1){
    U_alert <- "+"
    L_alert <- "-"
  }else if(U_count>1 & L_count<1){
    U_alert <- "+"
    L_alert <- ""
  }else if(U_count<1 & L_count>1){
    U_alert <- ""
    L_alert <- "-"
  }else{
    U_alert <- ""
    L_alert <- ""
  }
  write.csv(M_data, paste(mon, "/", cname, ".csv", sep=""))
  return(c(mean, std, upper,lower, U_count, L_count, U_alert, L_alert))
}
Mon <- "Q1" ##### modify the month here
A <- as.data.frame(lapply(clist, trend_mon, Mon))
colnames(A) <- clist
B <- t(A)
colnames(B) <- c("mean", "std", "upper", "lower", "U_count", "L_count", "U_alert", "L_alert")
C <- as.data.frame(B)
lan_list<- c("en-au", "", "", "", "cs-cz", "", "fr-fr", "de-de", "", "zh-hk", "", "en-in", "", "", "it-it", "ja-jp", "es-mx", "nl-nl", "", "pl-pl", "pt-pt", "", "", "ko-kr", "es-es", "sv-se", "", "zh-tw", "th-th", "", "en-uk", "en-us")
C$lan <- lan_list
as.data.frame(C)
write.csv(C, paste(Mon, "/stat_lan_v4.csv", sep=""))
